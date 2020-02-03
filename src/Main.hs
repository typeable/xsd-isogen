{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main
  ( main
  ) where

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
import Debug.Trace
import System.IO
import Text.XSD
import Options
import Data.Maybe


makeNamePrefix :: Text -> Text
makeNamePrefix = T.map C.toLower . T.filter (\c -> C.isUpper c || C.isDigit c)

type GenMonad = ReaderT DatatypeMap (Writer Text)

class Fields a where
  fields :: a -> [Text]

instance Fields Datatype where
  fields (TypeComplex ty) = fields ty
  fields (TypeSimple ty) = fields ty

instance Fields SimpleType where
  fields ty@(STAtomic tyName _ _ restrictions) = if shouldBeNewtype ty
    then [getNewtypeUnconsName tyName]
    else getEnumeratedCode restrictions

instance Fields ComplexType where
  fields (ComplexType _ _ _ (Just g)) = fields g
  fields (ComplexType _ _ _ Nothing) = []

instance Fields ModelGroupSchema where
  fields (CTSequence els) = snd . name <$> els
  fields (CTChoice els) = snd . name <$> els
  fields (CTAll els) = snd . name <$> els

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

-- | 'DatatypeMap' are the types left to process
-- 'S.Set Text' are the types already processed.
type PreMonad = StateT (DatatypeMap, S.Set Text) (Writer [(Text, Datatype)])

type PreElemMonad = RWS DatatypeMap [Element] [Element]

-- | Marks the given type as processed.
proceed :: Text -> Datatype -> PreMonad ()
proceed t dt = do
  tell [(t, dt)]
  modify $ \(f,s) -> (M.delete t f, S.insert t s)

-- | Marks element as processed.
proceed' :: Element -> PreElemMonad ()
proceed' el = do
  tell [el]
  modify $ L.delete el

untwistDeps :: [Element] -> DatatypeMap -> ([(Text, Datatype)], [Element])
untwistDeps e dm = (dList, e')
  where
    dList   = disambiguateNames . nub . execWriter $ do
      (a, _) <- runStateT sortDatatypesByDeps (dm, S.empty)
      return a
    (_, e') = execRWS sortElemsByDeps (M.fromList dList) e

type Abbreviation = Text
type Subelement = Text
type DisambiguaterMonad = State (M.Map Abbreviation (S.Set Subelement))

-- | If any two datatypes with the same name abbreviation ('makeNamePrefix')
-- have any fields in common, then one of them gets a \"2\" appended to it
-- (\"_2\" if the last character of the name was a digit).
-- Repeat the same process until there are no conflicts.
-- If we have already appended a \"2\" to a datatype and it still has conflicts,
-- we change the 2 to a 3, a 3 to a 4 and so on.
disambiguateNames :: [(Text, Datatype)] -> [(Text, Datatype)]
disambiguateNames = flip evalState mempty . mapM disambiguateName
  where
    disambiguateName :: (Text, Datatype) -> DisambiguaterMonad (Text, Datatype)
    disambiguateName (tyName', dt) = do
      let rNames = S.fromList $ fields dt
      (, dt) <$> attempt tyName' rNames 1
      where
        attempt :: Abbreviation -> S.Set Subelement -> Int -> DisambiguaterMonad Text
        attempt tyName fieldNames n = do
          processedTypes <- get
          let
            newTyName
              | n == 1 = tyName
              | shouldSeparateNumber = tyName <> "_" <> T.pack (show n)
              | otherwise = tyName <> T.pack (show n)
            abr = makeNamePrefix newTyName
          case M.lookup abr processedTypes of
            Just usedNames ->
              if S.null $ S.intersection fieldNames usedNames
                then modify (M.adjust (S.union fieldNames) abr) >> return newTyName
                else attempt tyName fieldNames (n + 1)
            Nothing -> modify (M.insert abr fieldNames) >> return newTyName
        shouldSeparateNumber = isDigit $ T.last tyName'

sortElemsByDeps :: PreElemMonad ()
sortElemsByDeps = do
  els <- get
  if Prelude.null els then pure () else do
    let first = Prelude.head els
    -- traceM $ "sortElemsByDeps: " <> show first
    case references first of
      [] -> proceed' first >> sortElemsByDeps
      xs -> do
        results <- traverse memberDatatype xs
        if L.and results
        then proceed' first >> sortElemsByDeps
        else do
          let failed = fmap snd $ L.filter (not . fst) $ L.zip results xs
          error $ "impossible to lookup types: " <> show failed

memberDatatype :: Text -> PreElemMonad Bool
memberDatatype ty = do
  -- traceM $ "memberDatatype: " <> show ty
  dm <- ask
  case M.lookup ty dm of
    Just _ -> return True
    Nothing  -> return False

sortDatatypesByDeps :: PreMonad ()
sortDatatypesByDeps = do
  (dm, _) <- get
  if M.null dm then pure () else do
    let first = Prelude.head (M.toList dm)
    traceM $ "sortDatatypesByDeps: " <> show first
    case references $ snd first of
      [] -> uncurry proceed first >> sortDatatypesByDeps
      xs -> do
        traverse_ lookupDatatype xs
        uncurry proceed first
        sortDatatypesByDeps

lookupDatatype :: Text -> PreMonad ()
lookupDatatype ty = do
  traceM $ "lookupDatatype: " <> show ty
  (dm, s) <- get
  case M.lookup ty dm of
    Nothing -> if S.member ty s then pure () else
      error $ "can't look up datatype: " <> show ty
    Just typ -> case references typ of
      [] -> proceed ty typ
      xs -> traverse_ lookupDatatype xs >> proceed ty typ

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
  STAnyURI            -> wrap fl "Text"
  STBase64Binary      -> wrap fl "ByteString"
  STBoolean           -> wrap fl "Bool"
  STDate              -> wrap fl "Date \"%YYYYj\""
  STDateTime          -> wrap fl "Text"
  STDateTimeStamp     -> wrap fl "Text"
  STDecimal DTDecimal -> wrap fl "Double"
  (STDecimal _)       -> wrap fl "Integer"
  STDouble            -> wrap fl "Double"
  (STDuration _)      -> wrap fl "Integer"
  STFloat             -> wrap fl "Float"
  STGDay              -> wrap fl "Day"
  STGMonth            -> wrap fl "Text"
  STGMonthDay         -> wrap fl "Text"
  STGYear             -> wrap fl "Text"
  STGYearMonth        -> wrap fl "Text"
  STHexBinary         -> wrap fl "Text"
  STNOTATION          -> wrap fl "Text"
  STQName             -> wrap fl "Text"
  (STString _)        -> wrap fl "Text"
  STTime              -> wrap fl "Text"
  where
    wrap QQ    t = "[t|" <> t <> "|]"
    wrap Plain t = t

tellGen
  :: [Text] -- generated code
  -> Text   -- comment
  -> GenMonad ()
tellGen code comment =
  tell $ "\n" <> comment <> T.unlines code

runGen :: DatatypeMap -> GenMonad a -> Text
runGen dm act = execWriter $ runReaderT act dm

generateIsoXml :: Maybe Header -> ModuleName -> GenType -> XSD -> Text
generateIsoXml mHeader name genType (XSD (e,dm)) =
  let
    (dList, el) = untwistDeps e dm
  in (header mHeader name genType <>) . runGen dm $ do
      for_ dList $ \(t,dt) ->
        generateIsoDatatype genType t dt []
      for_ el (generateIsoRecord genType)

toQualifier :: Int -> Int -> Text
toQualifier minOc maxOc
  | minOc == 1 && maxOc == 1 = "!"
  | minOc == 0 && maxOc == 1 = "?"
  | minOc == 1 && maxOc > minOc = "+"
  | minOc == 1 && maxOc < minOc = error "maxOccurs < minOccurs"
  | otherwise = "*"

header :: Maybe Header -> ModuleName -> GenType -> Text
header mHeader name genType = T.unlines $
  maybeToList (T.unlines . fmap ("-- " <>) . T.lines <$> mHeader) <>
  [ "module " <> name <> " where"
  , ""
  , "import Control.DeepSeq"
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

generateIsoRecord :: GenType -> Element -> GenMonad ()
generateIsoRecord
  genType
  (Element (_, laxName) elXtype _ _ elAnnotations) = do
    written <- ask
    case elXtype of
      InlineComplex dt ->
        generateIsoDatatype genType laxName dt elAnnotations
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
      in if T.length last' + T.length e >= 73 -- 80 minus "-- | " and ' '
      then l <> pure e
      else L.init l <> [L.last l <> " " <> e]

generateIsoDatatype
  :: GenType
  -> Text
  -> Datatype
  -> [Annotation]
  -> GenMonad ()
  -- ^ (type name for lookups, generated code)
generateIsoDatatype
  genType
  eName
  (TypeComplex (ComplexType _ _ tyAnnotations mGroupSchema))
  annotationsRec =
    case mGroupSchema of
      Just groupSchema -> case groupSchema of
        CTSequence elements -> do
          tyFields <- for elements (generateIsoField genType)
          let
            comment = toComment Haddock $ annotationsRec
              <> [Documentation ""]
              <> tyAnnotations
            tyHeader  =
              quote (capitalize eName)
              <> " =:= record "
              <> genTypeToText genType
          tellGen (tyHeader : tyFields) comment
        CTChoice _ -> error "'Choice' group not supported yet"
        CTAll _ -> error "'All' group not supported yet"
      Nothing -> error $ "No group schema provided for type: " <> T.unpack eName
generateIsoDatatype
  genType
  eName
  (TypeSimple ty@(STAtomic iName sat tyAnnotations restrictions))
  annotationsRec = do
    let
      comment                         =
        toComment Haddock $ annotationsRec
          <> [Documentation ""]
          <> tyAnnotations
    if not (shouldBeNewtype ty)
    then tellGen
      (generateEnum
        (quote $ capitalize eName)
        (getEnumeratedCode restrictions))
      comment
    else
      tellGen (generateNewtype genType iName sat) comment

getEnumeratedCode :: [Restriction] -> [Text]
getEnumeratedCode = F.concatMap fromEnumeration . Prelude.filter isEnumeration
  where
    fromEnumeration (Enumeration x) = x

isEnumeration :: Restriction -> Bool
isEnumeration (Enumeration _) = True

shouldBeNewtype :: SimpleType -> Bool
shouldBeNewtype (STAtomic _ _ _ restrictions) =
  not . Prelude.any isEnumeration $ restrictions

getNewtypeConsName :: Text -> Text
getNewtypeConsName = ("Xml" <>) . capitalize

getNewtypeUnconsName :: Text -> Text
getNewtypeUnconsName = ("un" <>) . getNewtypeConsName

generateNewtype :: GenType -> Text -> SimpleAtomicType -> [Text]
generateNewtype genType n sat =
  let
    constructor = getNewtypeConsName n
    decons      = getNewtypeUnconsName n
    parser      =
      [ ""
      , "instance FromDom " <> constructor <> " where"
      , "  fromDom = " <> constructor <> " <$> fromDom"
      ]
  in
    [ "newtype " <> constructor
    , "  = " <> constructor
    , "  { " <> decons <> " :: " <> simpleTypeToIsoType Plain sat
    , "  } deriving (Show, Eq, NFData" <> case genType of
      Parser -> ")"
      _      -> ", ToXML)"
    ] <> case genType of
       Generator -> []
       _         -> parser

generateEnum
  :: Text                -- type name
  -> [Text]              -- enums
  -> [Text]              -- generated code
generateEnum enumName enums =
  let
   enumHeader  = enumName <> " Exhaustive =:= enum Both"
   eFields = ("& "<>) <$> enums
  in pure enumHeader <> eFields

lookupSimpleType
  :: DatatypeRef
  -> GenMonad (Maybe Text)
lookupSimpleType (InlineComplex  _) = pure Nothing
lookupSimpleType (DatatypeRef tyName) =
  asks (M.lookup tyName) >>= \case
    Just (TypeComplex _)                   -> pure Nothing
    Just (TypeSimple (STAtomic nam _ _ _)) -> pure $ Just nam
    Nothing                                -> error $ "can't look up type: " <> show tyName

generateIsoField :: GenType -> Element -> GenMonad Text
generateIsoField
  genType
  (Element (_, fieldName) fieldXtype minOc maxOc fieldAnnotations) = do
    let
      qualifier = toQualifier minOc maxOc
    line <- lookupSimpleType fieldXtype >>= \case
      Just tyName ->
        pure $ quote fieldName <> " [t|Xml" <> tyName <> "|]"
      Nothing           -> case fieldXtype of
        InlineComplex dt -> do
          generateIsoDatatype genType (capitalize fieldName) dt fieldAnnotations
          pure $ quote fieldName
        DatatypeRef ref ->
          pure $ quote fieldName <> " [t|Xml" <> capitalize ref <> "|]"
    pure $ "  " <> qualifier <> " " <> line

main :: IO ()
main = do
  Options{..} <- getOptions
  xml <- BL.readFile oInput
  let
    xsd = case parseXSD def xml of
      Right xsd' -> xsd'
      Left e    -> error $ show e
  T.hPutStrLn stdout $ generateIsoXml oHeader oModule oMode xsd
