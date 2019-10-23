{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.RWS.CPS (RWS, execRWS)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.CPS (Writer, tell, execWriter)
import Data.ByteString.Lazy as BL
import Data.Char as C
import Data.Default
import Data.Foldable as F
import Data.List as L
import Data.Map.Strict as M
import Data.Set as S
import Data.Text as T
import Data.Text.IO as T
import Data.Traversable
import System.Environment
import System.IO
import Text.XSD


data GenType = Parser | Generator | Both
  deriving (Read, Show)

genTypeToText :: GenType -> Text
genTypeToText Parser    = "Parser"
genTypeToText Generator = "Generator"
genTypeToText Both      = "Both"

type GenMonad = ReaderT DatatypeMap (Writer Text)

class References a where
  references :: a -> [Text]

instance References Datatype where
  references (TypeComplex (ComplexType _ _ _ mmgs)) =
    case mmgs of
      Just mgs -> case mgs of
        CTSequence els -> F.concatMap references els
        CTChoice   els -> F.concatMap references els
        CTAll      els -> F.concatMap references els
      Nothing -> []
  references (TypeSimple _) = []

instance References Element where
  references (Element _ t _ _ _) = references t

instance References DatatypeRef where
  references (DatatypeRef ty)   = [ty]
  references (InlineComplex dt) = references dt

instance References a => References [a] where
  references = F.concatMap references

type PreMonad = StateT (DatatypeMap, S.Set Text) (Writer [(Text, Datatype)])

type PreElemMonad = RWS DatatypeMap [Element] [Element]

proceed :: Text -> Datatype -> PreMonad ()
proceed t dt = do
  tell [(t, dt)]
  modify $ \(f,s) -> (M.delete t f, S.insert t s)

proceed' :: Element -> PreElemMonad ()
proceed' el = do
  tell [el]
  modify $ L.delete el

untwistDeps :: [Element] -> DatatypeMap -> ([(Text, Datatype)], [Element])
untwistDeps e dm = (dList, e')
  where
    dList   = execWriter $ do
      (a, (_,s)) <- runStateT sortDatatypesByDeps (dm, S.empty)
      return a
    (_, e') = execRWS sortElemsByDeps (M.fromList dList) e

sortElemsByDeps :: PreElemMonad ()
sortElemsByDeps = do
  els <- get
  if Prelude.null els then pure () else do
    let first = Prelude.head els
    case references first of
      [] -> proceed' first >> sortElemsByDeps
      xs -> do
        results <- traverse memberDatatype xs
        if L.all id results
        then proceed' first >> sortElemsByDeps
        else do
          let failed = fmap snd $ L.filter (not . fst) $ L.zip results xs
          error $ "impossible to lookup types: " <> show failed

memberDatatype :: Text -> PreElemMonad Bool
memberDatatype ty = do
  dm <- ask
  case M.lookup ty dm of
    Just typ -> return True
    Nothing  -> return False

sortDatatypesByDeps :: PreMonad ()
sortDatatypesByDeps = do
  (dm, s) <- get
  if M.null dm then pure () else do
    let first = Prelude.head (M.toList dm)
    case references $ snd first of
      [] -> uncurry proceed first >> sortDatatypesByDeps
      xs -> do
        traverse_ lookupDatatype xs
        uncurry proceed first
        sortDatatypesByDeps

lookupDatatype :: Text -> PreMonad ()
lookupDatatype ty = do
  (dm, s) <- get
  let mTy = M.lookup ty dm
  case M.lookup ty dm of
    Nothing -> if S.member ty s then pure () else do
      error $ "can't look up datatype: " <> show ty
    Just typ -> case references typ of
      [] -> proceed ty typ >> sortDatatypesByDeps
      xs -> traverse_ lookupDatatype xs

class Capitalizable t where
  capitalize :: t -> t

instance Capitalizable String where
  capitalize []     = []
  capitalize (a:as) = C.toUpper a : as

instance Capitalizable Text where
  capitalize t
    | T.null t  = t
    | otherwise = T.singleton (C.toUpper (T.head t)) <> T.tail t

data DataFlavour = QQ | Plain

-- TODO: improve after moving some XML types to xml-isogen
simpleTypeToIsoType :: DataFlavour -> SimpleAtomicType -> Text
simpleTypeToIsoType fl = \case
  STAnyURI        -> wrap fl "Text"
  STBase64Binary  -> wrap fl "ByteString"
  STBoolean       -> wrap fl "Bool"
  STDate          -> wrap fl "Date \"%YYYYj\""
  STDateTime      -> wrap fl "Text"
  STDateTimeStamp -> wrap fl "Text"
  (STDecimal _)   -> wrap fl "Integer"
  STDouble        -> wrap fl "Double"
  (STDuration _)  -> wrap fl "Integer"
  STFloat         -> wrap fl "Float"
  STGDay          -> wrap fl "Day"
  STGMonth        -> wrap fl "Text"
  STGMonthDay     -> wrap fl "Text"
  STGYear         -> wrap fl "Text"
  STGYearMonth    -> wrap fl "Text"
  STHexBinary     -> wrap fl "Text"
  STNOTATION      -> wrap fl "Text"
  STQName         -> wrap fl "Text"
  (STString _)    -> wrap fl "Text"
  STTime          -> wrap fl "Text"
  where
    wrap QQ    t = "[t|" <> t <> "|]"
    wrap Plain t = t

tellGen
  :: [Text] -- generated code
  -> Text   -- comment
  -> GenMonad ()
tellGen code comment = do
  tell $ "\n" <> comment <> T.unlines code

runGen :: DatatypeMap -> GenMonad a -> Text
runGen dm act = execWriter $ runReaderT act dm

generateIsoXml :: GenType -> XSD -> Text
generateIsoXml genType xsd@(XSD (e,d)) =
  let
    (dList, el) = untwistDeps e d
  in (header genType <>) . runGen d $ do
      for_ dList $ \(t,d) -> do
        generateIsoDatatype genType (Just t) d []
      for_ el (generateIsoRecord genType)

toQualifier :: Int -> Maybe Int -> Text
toQualifier minOc (Just maxOc)
  | minOc == 1 && maxOc == 1 = "!"
  | minOc == 0 && maxOc == 1 = "?"
  | minOc == 1 && maxOc > minOc = "+"
  | minOc == 1 && maxOc < minOc = error "maxOccurs < minOccurs"
  | otherwise = "*"
toQualifier _ Nothing = "*"

header :: GenType -> Text
header genType = T.unlines $
  [ "{-# LANGUAGE DuplicateRecordFields #-}"
  , ""
  , "module Dummy where"
   ""
  , "import Data.THGen.XML"
  ] <> specImports <>
  [ "import Prelude hiding ((*), (+))"
  , "" ]
  where
    specImports = case genType of
      Generator -> [ xmlWriter ]
      Parser    -> [ domParser ]
      Both      -> [ domParser, xmlWriter ]
    xmlWriter, domParser :: Text
    xmlWriter = "import Text.XML.Writer"
    domParser = "import Text.XML.DOM.Parser.FromDom"

quote :: Text -> Text
quote t = "\"" <> t <> "\""

-- | Nothing means unknown name
typeName :: DatatypeRef -> Maybe Text
typeName (InlineComplex dt) = result
  where
    result = case dt of
      TypeComplex (ComplexType n _ _ _) -> n
      TypeSimple (STAtomic _ sat _ _)     -> Just $ simpleTypeToIsoType QQ sat
typeName (DatatypeRef t)               = Just t

generateIsoRecord :: GenType -> Element -> GenMonad ()
generateIsoRecord
  genType
  (Element (_, laxName) xtype minOc maxOc annotations) = do
    written <- ask
    case xtype of
      InlineComplex dt ->
        generateIsoDatatype genType (Just laxName) dt annotations
      DatatypeRef nam  -> if M.member nam written
        then pure ()
        else error $ "Implementation failure: "
          <> show nam <> " type never encountered before"

data CommentType = Haddock | Regular

toComment :: CommentType -> [Annotation] -> Text
toComment ct ann = case texts of
  []     -> ""
  (h:tl) -> T.unlines ((prefix <> h) : fmap ("-- "<>) tl)
  where
    texts      = stripTexts $ F.concatMap (wrap80 . fromDocumentation) ann
    stripTexts = \case
      []       -> []
      l@(h:tl) -> if T.all (\x -> x == ' ' || x == '\n') h then stripTexts tl else l
    prefix     = case ct of
      Haddock -> "-- | "
      Regular -> "-- "

fromDocumentation :: Annotation -> Text
fromDocumentation (Documentation t) = t

wrap80 :: Text -> [Text]
wrap80 = L.foldl' go [] . T.split (==' ')
  where
    go [] e = [e]
    go l  e =
      let last'    = L.last l
      in if (T.length last') + T.length e >= 73 -- 80 minus "-- | " and ' '
      then l <> pure e
      else L.init l <> [L.last l <> " " <> e]

generateIsoDatatype
  :: GenType
  -> (Maybe Text) -- ^ Explicitly provided name, used for inline complexType's
  -> Datatype
  -> [Annotation]
  -> GenMonad ()
  -- ^ (type name for lookups, generated code)
generateIsoDatatype
  genType
  eName
  (TypeComplex (ComplexType iName attrs annotations mGroupSchema))
  annotationsRec = do
    name <- case eName of
      Just name -> pure name
      Nothing   -> case iName of
        Just name' -> pure name'
        Nothing    -> error $ "[generateIsoDatatype] no name was provided"
    case mGroupSchema of
      Just groupSchema -> case groupSchema of
        CTSequence elems -> do
          fields <- for elems (generateIsoField genType)
          let
            comment = toComment Haddock $ annotationsRec
              <> [Documentation ""]
              <> annotations
            header  =
              (quote $ capitalize name)
              <> " =:= record "
              <> genTypeToText genType
          tellGen (header : fields) comment
generateIsoDatatype
  genType
  eName
  (TypeSimple (STAtomic iName sat annotations restrictions))
  annotationsRec = do
    name <- case eName of
      Just name -> pure name
      Nothing   -> pure iName
    let
      comment                         =
        toComment Haddock $ annotationsRec
          <> [Documentation ""]
          <> annotations
      isEnumeration (Enumeration _)   = True
      fromEnumeration (Enumeration x) = x
      enums                           = Prelude.filter isEnumeration restrictions
      typeStr                         = simpleTypeToIsoType QQ sat
    if Prelude.length enums > 0
    then tellGen
      (generateEnum
        (quote $ capitalize name)
        (F.concatMap fromEnumeration enums))
      comment
    else do
      tellGen (generateNewtype genType iName sat) comment

generateNewtype :: GenType -> Text -> SimpleAtomicType -> [Text]
generateNewtype genType n sat =
  let
    cons   = "Xml" <> capitalize n
    decons = "un" <> cons
    parser =
      [ "instance FromDom " <> cons <> " where"
      , "  fromDom = " <> cons <> " fromDom"
      ]
  in
    [ "newtype " <> cons
    , "  = " <> cons
    , "  { " <> decons <> " :: " <> simpleTypeToIsoType Plain sat
    , "  } deriving (Show, Eq" <> case genType of
      Parser -> ")"
      _      -> ", ToXML)"
    ] <> case genType of
       Generator -> []
       _         -> parser


generateEnum
  :: Text                -- type name
  -> [Text]              -- enums
  -> [Text]              -- generated code
generateEnum name enums =
  let
   header  = name <> " Exhaustive =:= enum Both"
   eFields = ("& "<>) <$> enums
  in pure header <> eFields

-- | Returns Just baseTypeName if type is simple, Nothing - otherwise
lookupSimpleBaseType
  :: DatatypeRef
  -> GenMonad (Maybe SimpleAtomicType)
lookupSimpleBaseType (InlineComplex  _) = pure Nothing
lookupSimpleBaseType (DatatypeRef tyName) = do
  M.lookup tyName <$> ask >>= \case
    Just (TypeComplex _)                 -> pure Nothing
    Just (TypeSimple (STAtomic _ sat _ _)) -> pure $ Just sat
    Nothing                              -> error $ "can't look up type: " <> show tyName

lookupSimpleType
  :: DatatypeRef
  -> GenMonad (Maybe Text)
lookupSimpleType (InlineComplex  _) = pure Nothing
lookupSimpleType (DatatypeRef tyName) = do
  M.lookup tyName <$> ask >>= \case
    Just (TypeComplex _)                   -> pure Nothing
    Just (TypeSimple (STAtomic nam _ _ _)) -> pure $ Just nam
    Nothing                              -> error $ "can't look up type: " <> show tyName

generateIsoField :: GenType -> Element -> GenMonad Text
generateIsoField genType (Element (_, name) xtype minOc maxOc annotations) = do
  let
    qualifier = toQualifier minOc maxOc
  line <- lookupSimpleType xtype >>= \case
    Just tyName -> do
      pure $ quote name <> " [t|Xml" <> tyName <> "|]"
    Nothing           -> case xtype of
      InlineComplex dt -> do
        generateIsoDatatype genType (Just $ capitalize name) dt annotations
        pure $ quote name
      DatatypeRef ref ->
        pure $ quote name <> " [t|Xml" <> capitalize ref <> "|]"
  pure $ "  " <> qualifier <> " " <> line

main :: IO ()
main = do
  (inFile:mode:_) <- getArgs
  xml <- BL.readFile inFile
  let Right xsd = parseXSD def xml
  T.hPutStrLn stdout $ generateIsoXml (read mode) xsd
