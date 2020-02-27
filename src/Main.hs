{-# LANGUAGE OverloadedStrings #-}

module Main
( main
)
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Maybe
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import qualified System.IO as IO
import qualified Xsd

import Options
import Gen
import References
import BuiltInTypes

main :: IO ()
main = do
  opts <- getOptions
  schema <- Xsd.getSchema (oInput opts)
  res <- either fail return
    (generate opts schema)
  mapM_ (Text.hPutStrLn IO.stderr) (resultWarnings res)
  Text.putStrLn (resultCode res)

data Result = Result
  { resultCode :: Text
  , resultWarnings :: [Text]
  }

generate :: Options -> Xsd.Schema -> Either String Result
generate opts schema = do
  let
    types = Map.union (Xsd.schemaTypes schema)
      (elementsToTypes (Xsd.schemaElements schema))
  s <- runGen (initialGenState opts types) $ do
    genHeader opts
    genTypes types
  Right Result
    { resultCode = Text.unlines (reverse (gsCode s))
    , resultWarnings = reverse (gsWarnings s)
    }

elementsToTypes :: Map Xsd.QName Xsd.Element -> Map Xsd.QName Xsd.Type
elementsToTypes = List.foldl' step Map.empty . Map.toList
  where
  step types (_, e) = case Xsd.elementType e of
    Xsd.Ref _ -> types
    Xsd.Inline t -> Map.insert (Xsd.elementName e) t types

genTypes :: Map Xsd.QName Xsd.Type -> Gen ()
genTypes types = mapM_ (uncurry genTopType) . Map.toList $ types

genTopType :: Xsd.QName -> Xsd.Type -> Gen ()
genTopType = genType []

genType :: [Xsd.Annotation] -> Xsd.QName -> Xsd.Type -> Gen ()
genType annotations name tp = do
  known <- knownTypeName name
  case known of
    Just _ -> return () -- already generated
    Nothing -> do
      -- we should generate dependencies before the type itself to make
      -- sure they are in scope when xml-isogen quotes them
      -- (stage restriction in TH)
      genDependencies tp
      typeName <- makeTypeName name tp
      case tp of
        Xsd.TypeSimple t -> genSimpleType typeName t annotations
        Xsd.TypeComplex t -> genComplexType typeName t annotations
      genExtension name

genExtension :: Xsd.QName -> Gen ()
genExtension base = do
  exts <- extensions base
  case exts of
    [] -> return ()
    types -> do
      mapM_ (uncurry genTopType) types
      baseTypeName <- tnPrefixed <$> resolveTypeName base
      writeCode
        [ "data Any" <> baseTypeName
        , "  = The" <> baseTypeName <> " !" <> baseTypeName
        ]
      let
        mkConstructor (n, _) = do
          tn <- tnPrefixed <$> resolveTypeName n
          exts' <- extensions n
          let
            n' = case exts' of
              [] -> tn
              _ -> "Any" <> tn
          return ("  | The" <> n' <> " !" <> n')
      constructors <- mapM mkConstructor types
      writeCode constructors
      writeCode
        [ "  deriving (Eq, Show)"
        , ""
        ]
      writeCode
        [ "instance NFData Any" <> baseTypeName <> " where"
        , "  rnf = rwhnf"
        , " "
        ]
      mode <- getMode
      unless (mode == Generator) $ do
        writeCode
          [ "instance FromDom Any" <> baseTypeName <> " where"
          , "  fromDom = do"
          , "    tp <- parseAttribute"
          , "      (matchName " <>
            "\"{http://www.w3.org/2001/XMLSchema-instance}type\")"
          , "      Right"
          , "    case tp of"
          ]
        let
          mkClause n = do
            tn <- resolveTypeName n
            exts' <- extensions n
            n' <- case exts' of
              [] -> return (tnPrefixed tn)
              _ | n == base -> return (tnPrefixed tn)
              _ -> return ("Any" <> tnPrefixed tn)
            return $ mconcat
              [ "      \""
              , tnName tn
              , "\" -> The"
              , n'
              , " <$> fromDom"
              ]
        clauses <- mapM mkClause (base : map fst types)
        writeCode clauses
        writeCode
          [ "      _ -> throwParserError (PEOther \"Unexpected type\")"
          , ""
          ]
      unless (mode == Parser) $ do
        writeCode
          [ "instance ToXML Any" <> baseTypeName <> " where"
          , "  toXML (The" <> baseTypeName <> " a) = toXML a"
          ]
        let
          mkClause (n, _) = do
            tn <- tnPrefixed <$> resolveTypeName n
            exts' <- extensions n
            let
              n' = case exts' of
                [] -> tn
                _ -> "Any" <> tn
            return ("  toXML (The" <> n' <> " a) = toXML a")
        clauses <- mapM mkClause types
        writeCode clauses
        writeCode [""]
        writeCode
          [ "instance ToXmlParentAttributes Any" <> baseTypeName <> " where"
          ]
        let
          toAttrLine n = do
            tn <- resolveTypeName n
            exts' <- extensions n
            n' <- case exts' of
              [] -> return (tnPrefixed tn)
              _ | n == base -> return (tnPrefixed tn)
              _ -> return ("Any" <> tnPrefixed tn)
            return
              [ "  toXmlParentAttributes (The" <> n' <> " a) ="
              , "    (\"{http://www.w3.org/2001/XMLSchema-instance}type\", \""
                <> tnName tn <> "\")"
              , "    : toXmlParentAttributes a"
              ]
        attrLines <- mapM toAttrLine (base : map fst types)
        writeCode (concat attrLines)

genDependencies :: References a => a -> Gen ()
genDependencies a = forM_ (references a) $ \name -> do
  known <- knownTypeName name
  case known of
    Just _ -> return () -- already generated
    Nothing -> do
      tp <- resolveType name
      genTopType name tp

genSimpleType :: TypeName -> Xsd.SimpleType -> [Xsd.Annotation]-> Gen ()
genSimpleType tn (Xsd.AtomicType restriction ann) parentAnn = do
  let
    baseName r = case Xsd.simpleRestrictionBase r of
      Xsd.Ref n -> Just n
      Xsd.Inline (Xsd.AtomicType r' _) -> baseName r'
      Xsd.Inline (Xsd.ListType _ _) -> Nothing
      Xsd.Inline (Xsd.UnionType _ _) -> Nothing
  case baseName restriction of
    Just _
      | isEnum restriction
      -> genEnum tn
        (Xsd.simpleRestrictionConstraints restriction)
        (parentAnn <> ann)
    Just base ->
      genNewtype tn base (parentAnn <> ann)
    _ -> genUnsupported tn
genSimpleType typeName (Xsd.ListType _ _) _ = genUnsupported typeName
genSimpleType typeName (Xsd.UnionType _ _) _ = genUnsupported typeName

genNewtype :: TypeName -> Xsd.QName -> [Xsd.Annotation] -> Gen ()
genNewtype tn base annots = do
  fieldTypeName <- tnPrefixed <$> resolveTypeName base
  mode <- getMode
  let
    typeName = tnPrefixed tn
    fieldName = "un" <> typeName
    toxml = case mode of
      Parser -> ""
      _ -> ", ToXML"
  writeCode $ makeComments tn annots
  writeCode
    [ "newtype " <> typeName
    , "  = " <> typeName
    , "  { " <> fieldName <> " :: " <> fieldTypeName
    , "  } deriving (Show, Eq, NFData" <> toxml <> ")"
    , ""
    ]
  unless (mode == Generator) $ writeCode
    [ "instance FromDom " <> typeName <> " where"
    , "  fromDom = " <> typeName <> " <$> fromDom"
    , ""
    ]

genEnum :: TypeName -> [Xsd.Constraint] -> [Xsd.Annotation] -> Gen ()
genEnum tn constraints annots = do
  mode <- getMode
  let
    values = map toValue constraints
    toValue (Xsd.Enumeration v) = v
  enumLines <- forM values $ \v -> do
    enumName <- makeEnumName v
    return ("  & \"" <> enumName <> "\"")
  writeCode $ makeComments tn annots
  writeCode $ "\"" <> tnName tn <> "\" Exhaustive =:= enum "
    <> genTypeToText mode : enumLines
  writeCode [""]

isEnum :: Xsd.SimpleRestriction -> Bool
isEnum = any isEnumConstraint . Xsd.simpleRestrictionConstraints
  where
  isEnumConstraint (Xsd.Enumeration _) = True

genComplexType :: TypeName -> Xsd.ComplexType -> [Xsd.Annotation] -> Gen ()
genComplexType typeName t parentAnn = do
  mode <- getMode
  let
    header = "\"" <> tnName typeName <> "\" =:= record "
      <> genTypeToText mode
  fields <- genFields typeName (Xsd.complexContent t)
  writeCode $ makeComments typeName
    (parentAnn <> Xsd.complexAnnotations t)
  writeCode [header]
  writeCode fields
  writeCode [""]

makeComments :: TypeName -> [Xsd.Annotation] -> [Text]
makeComments typeName annotations = if null annotations
  then
    [ "{-|"
    , generated
    , "-}"
    ]
  else
    [ "{-|"
    , generated
    , ""
    ] <> map makeDoc annotations <>
    [ "-}"
    ]
  where
  generated = "Generated from: " <> prefix <> name
  qname = tnQName typeName
  name = Xsd.qnName qname
  prefix = maybe "" ((<> ":") . Xsd.fromNamespace) (Xsd.qnNamespace qname)
  makeDoc (Xsd.Documentation docs) = docs

-- | Generate code for record fields
--
-- The first argument - record name
genFields :: TypeName -> Xsd.Content -> Gen [Text]
genFields typeName (Xsd.ContentPlain c) =
  maybe (return []) (genFieldsForModel typeName) (Xsd.plainContentModel c)
genFields _ (Xsd.ContentSimple _) = return []
genFields typeName (Xsd.ContentComplex (Xsd.ComplexContentExtension e)) = do
  base <- resolveType (Xsd.complexExtensionBase e)
  baseFields <- case base of
    Xsd.TypeComplex t -> genFields typeName (Xsd.complexContent t)
    _ -> return []
  fields <- maybe (return []) (genFieldsForModel typeName)
    (Xsd.complexExtensionModel e)
  return (baseFields ++ fields)
genFields _ (Xsd.ContentComplex (Xsd.ComplexContentRestriction _)) =
  return []

genFieldsForModel :: TypeName -> Xsd.ModelGroup -> Gen [Text]
genFieldsForModel typeName (Xsd.Sequence elements) =
  catMaybes <$> mapM (genField typeName) elements
genFieldsForModel _ (Xsd.Choice _) = do
  warn "choice: not implemented"
  return []
genFieldsForModel _ (Xsd.All _) = do
  warn "all: not implemented"
  return []

genField :: TypeName -> Xsd.RefOr Xsd.Element -> Gen (Maybe Text)
genField _ (Xsd.Ref _) = return Nothing
genField typeName (Xsd.Inline e) = do
  fieldName <- makeFieldName typeName (Xsd.elementName e)

  fieldTypeName <- case Xsd.elementType e of
    Xsd.Ref name -> do
      tn <- tnPrefixed <$> resolveTypeName name
      exts <- extensions name
      case exts of
        [] -> return tn
        _ -> return ("Any" <> tn)
    Xsd.Inline t -> do
      genType (Xsd.elementAnnotations e) (Xsd.elementName e) t
      tnPrefixed <$> resolveTypeName (Xsd.elementName e)

  qualifier <- makeQualifier (Xsd.elementOccurs e)

  let
    fieldType = if Xsd.elementNillable e
      then "Nillable " <> fieldTypeName
      else fieldTypeName

  return $ Just $ mconcat
    [ "  "
    , qualifier
    , " \""
    , fieldName
    , "\" [t|"
    , fieldType
    , "|]"
    ]

makeQualifier :: (Int, Xsd.MaxOccurs) -> Gen Text
makeQualifier (minOcc, maxOcc) = case maxOcc of
  Xsd.MaxOccursUnbound -> toQualifier minOcc maxBound
  Xsd.MaxOccurs v -> toQualifier minOcc v

toQualifier :: Int -> Int -> Gen Text
toQualifier 1 1 = return "!"
toQualifier 0 1 = return "?"
toQualifier 1 maxOcc
  | maxOcc < 1
  = genError "maxOccurs < minOccurs"
toQualifier 1 maxOcc
  | maxOcc > 1
  = return "+"
toQualifier _ _ = return "*"

genUnsupported :: TypeName -> Gen ()
genUnsupported tn = do
  let typeName = tnPrefixed tn
  warn $ "generating stub for unsupported construct in " <> typeName
  mode <- getMode
  writeCode (makeComments tn [])
  writeCode
    [ "data " <> typeName <> " = " <> typeName
    , "  deriving (Eq, Show)"
    , ""
    , "instance NFData " <> typeName <> " where"
    , "  rnf _ = ()"
    , ""
    ]

  unless (mode == Parser) $ writeCode
    [ "instance ToXML " <> typeName <> " where"
    , "  toXML _ = comment \"" <> typeName <> "\""
    , ""
    ]

  unless (mode == Generator) $ writeCode
    [ "instance FromDom " <> typeName <> " where"
    , "  fromDom = undefined"
    , ""
    ]

genHeader :: Options -> Gen ()
genHeader opts = do
  case oHeader opts of
    Nothing -> return ()
    Just h -> do
      writeCode . fmap ("-- " <>) . Text.lines $ h
      writeCode [""]

  writeCode
    [ "{-# OPTIONS_GHC -Wno-unused-imports #-}"
    , "module " <> oModule opts <> " where"
    , ""
    , "import Control.DeepSeq"
    , "import Data.Scientific"
    , "import Data.THGen.XML"
    ]
  writeCode imports
  writeCode
    [ "import Prelude hiding ((*), (+))"
    , ""
    , ""
    ]
  where
  imports = case oMode opts of
    Generator          -> [ xmlWriter, parentAttr ]
    Parser             -> [ domParser ]
    ParserAndGenerator -> [ domParser, xmlWriter, parentAttr ]
  xmlWriter  = "import Text.XML.Writer"
  parentAttr = "import Text.XML.ParentAttributes"
  domParser  = "import Text.XML.DOM.Parser"

-- | We populate the initial state with all built-in types to make sure
-- they will be correctly resolved
initialGenState :: Options -> Map Xsd.QName Xsd.Type -> GenState
initialGenState opts types = GenState
  { gsTypes = types
  , gsExtensions = collectExtensions types
  , gsKnownTypes = builtInTypes
  , gsBoundTypeName = Set.empty
  , gsBoundFields = Set.empty
  , gsMode = oMode opts
  , gsCode = []
  , gsWarnings = []
  }

collectExtensions
  :: Map Xsd.QName Xsd.Type
  -> Map Xsd.QName [(Xsd.QName, Xsd.Type)]
collectExtensions = List.foldl' step Map.empty . Map.toList
  where
  step exts (n, t) = case baseOf t of
    Nothing -> exts
    Just base -> Map.insertWith (++) base [(n, t)] exts
  baseOf (Xsd.TypeSimple _) = Nothing
  baseOf (Xsd.TypeComplex t) = case Xsd.complexContent t of
    Xsd.ContentComplex (Xsd.ComplexContentExtension e) ->
      Just (Xsd.complexExtensionBase e)
    _ -> Nothing
