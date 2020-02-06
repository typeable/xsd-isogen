{-# LANGUAGE OverloadedStrings #-}

module Main
( main
)
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Maybe
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
  s <- runGen (initialGenState opts schema) $ do
    genHeader opts
    genTypes (Xsd.schemaTypes schema)
    genElements (Xsd.schemaElements schema)
  Right Result
    { resultCode = Text.unlines (reverse (gsCode s))
    , resultWarnings = reverse (gsWarnings s)
    }

genTypes :: Map Xsd.QName Xsd.Type -> Gen ()
genTypes types = mapM_ (uncurry genTopType) . Map.toList $ types

genElements :: Map Xsd.QName Xsd.Element -> Gen ()
genElements = mapM_ genElement

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
    baseName r = case Xsd.restrictionBase r of
      Xsd.Ref n -> Just n
      Xsd.Inline (Xsd.AtomicType r' _) -> baseName r'
      Xsd.Inline (Xsd.ListType _ _) -> Nothing
  case baseName restriction of
    Just _
      | isEnum restriction
      -> genEnum tn (Xsd.restrictionConstraints restriction) (parentAnn <> ann)
    Just base ->
      genNewtype tn base (parentAnn <> ann)
    _ -> genUnsupported tn
genSimpleType typeName (Xsd.ListType _ _) _ = genUnsupported typeName

genNewtype :: TypeName -> Xsd.QName -> [Xsd.Annotation] -> Gen ()
genNewtype tn base annots = do
  fieldTypeName <- resolveTypeName base
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
  let
    values = map toValue constraints
    toValue (Xsd.Enumeration v) = v
  enumLines <- forM values $ \v -> do
    enumName <- makeEnumName v
    return ("  & \"" <> enumName <> "\"")
  writeCode $ makeComments tn annots
  writeCode $ "\"" <> tnName tn <> "\" Exhaustive =:= enum Both"
    : enumLines
  writeCode [""]

isEnum :: Xsd.Restriction -> Bool
isEnum = any isEnumConstraint . Xsd.restrictionConstraints
  where
  isEnumConstraint (Xsd.Enumeration _) = True

genComplexType :: TypeName -> Xsd.ComplexType -> [Xsd.Annotation] -> Gen ()
genComplexType typeName t parentAnn = do
  mode <- getMode
  let
    header = "\"" <> tnName typeName <> "\" =:= record "
      <> genTypeToText mode
  fields <- maybe (return []) (genFields typeName) (Xsd.complexModelGroup t)
  writeCode $ makeComments typeName
    (parentAnn <> Xsd.complexAnnotations t)
  writeCode [header]
  writeCode fields
  writeCode [""]

makeComments :: TypeName -> [Xsd.Annotation] -> [Text]
makeComments typeName annotations =
  [ "{-"
  , "Generated from: " <> prefix <> name
  , ""
  ] <> map makeDoc annotations <>
  [ "-}"
  , ""
  ]
  where
  qname = tnQName typeName
  name = Xsd.qnName qname
  prefix = maybe "" ((<> ":") . Xsd.fromNamespace) (Xsd.qnNamespace qname)
  makeDoc (Xsd.Documentation docs) = docs

-- | Generate code for record fields
--
-- The first argument - record name
genFields :: TypeName -> Xsd.ModelGroup -> Gen [Text]
genFields typeName (Xsd.Sequence elements) =
  catMaybes <$> mapM (genField typeName) elements
genFields _ (Xsd.Choice _) = do
  warn "choice: not implemented"
  return []
genFields _ (Xsd.All _) = do
  warn "all: not implemented"
  return []

genField :: TypeName -> Xsd.Element -> Gen (Maybe Text)
genField typeName e = do
  fieldName <- makeFieldName typeName (Xsd.elementName e)

  fieldTypeName <- case Xsd.elementType e of
    Xsd.Ref name -> resolveTypeName name
    Xsd.Inline t -> do
      genType (Xsd.elementAnnotations e) (Xsd.elementName e) t
      resolveTypeName (Xsd.elementName e)

  qualifier <- makeQualifier (Xsd.elementOccurs e)

  return $ Just $ mconcat
    [ "  "
    , qualifier
    , " \""
    , fieldName
    , "\" [t|"
    , fieldTypeName
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

genElement :: Xsd.Element -> Gen ()
genElement e = case Xsd.elementType e of
  Xsd.Ref _ -> return ()
  Xsd.Inline t -> genType (Xsd.elementAnnotations e) (Xsd.elementName e) t

genHeader :: Options -> Gen ()
genHeader opts = do
  case oHeader opts of
    Nothing -> return ()
    Just h -> do
      writeCode . fmap ("-- " <>) . Text.lines $ h
      writeCode [""]

  writeCode
    [ "module " <> oModule opts <> " where"
    , ""
    , "import Control.DeepSeq"
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
    Generator -> [ xmlWriter ]
    Parser    -> [ domParser ]
    Both      -> [ domParser, xmlWriter ]
  xmlWriter = "import Text.XML.Writer"
  domParser = "import Text.XML.DOM.Parser.FromDom"

-- | We populate the initial state with all built-in types to make sure
-- they will be correctly resolved
initialGenState :: Options -> Xsd.Schema -> GenState
initialGenState opts schema = GenState
  { gsTypes = Xsd.schemaTypes schema
  , gsKnownTypes = builtInTypes
  , gsBoundTypeName = Set.empty
  , gsBoundFields = Set.empty
  , gsMode = oMode opts
  , gsCode = []
  , gsWarnings = []
  }
