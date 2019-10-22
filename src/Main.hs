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


type GenMonad = StateT (M.Map Text Type) (Writer Text)

class References a where
  references :: a -> [Text]

instance References Datatype where
  references (TypeComplex (ComplexType _ _ mmgs)) =
    case mmgs of
      Just mgs -> case mgs of
        CTSequence els -> F.concatMap references els
        CTChoice   els -> F.concatMap references els
        CTAll      els -> F.concatMap references els
      Nothing -> []
  references (TypeSimple _) = []

instance References Element where
  references (Element _ t _ _) = references t

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

data Type
  = Simple !SimpleAtomicType -- type to find iso correspondence
  | Complex

capitalize :: String -> String
capitalize [] = []
capitalize (a:as) = C.toUpper a : as

-- TODO: improve after moving some XML types to xml-isogen
simpleTypeToIsoType :: SimpleAtomicType -> Text
simpleTypeToIsoType STAnyURI        = "[t|Text|]"
simpleTypeToIsoType STBase64Binary  = "[t|ByteString|]"
simpleTypeToIsoType STBoolean       = "[t|Bool|]"
simpleTypeToIsoType STDate          = "[t|Date \"%YYYYj\"|]"
simpleTypeToIsoType STDateTime      = "[t|Text|]"
simpleTypeToIsoType STDateTimeStamp = "[t|Text|]"
simpleTypeToIsoType (STDecimal _)   = "[t|Integer|]"
simpleTypeToIsoType STDouble        = "[t|Double|]"
simpleTypeToIsoType (STDuration _)  = "[t|Integer|]"
simpleTypeToIsoType STFloat         = "[t|Float|]"
simpleTypeToIsoType STGDay          = "[t|Day|]"
simpleTypeToIsoType STGMonth        = "[t|Text|]"
simpleTypeToIsoType STGMonthDay     = "[t|Text|]"
simpleTypeToIsoType STGYear         = "[t|Text|]"
simpleTypeToIsoType STGYearMonth    = "[t|Text|]"
simpleTypeToIsoType STHexBinary     = "[t|Text|]"
simpleTypeToIsoType STNOTATION      = "[t|Text|]"
simpleTypeToIsoType STQName         = "[t|Text|]"
simpleTypeToIsoType (STString _)    = "[t|Text|]"
simpleTypeToIsoType STTime          = "[t|Text|]"

tellGen
  :: Text -- type
  -> Type
  -> [Text] -- generated code
  -> GenMonad ()
tellGen tyName ty code = do
  tell $ "\n" <> T.unlines code
  modify $ M.insert tyName ty

runGen :: GenMonad a -> Text
runGen = execWriter . flip evalStateT M.empty

generateIsoXml :: XSD -> Text
generateIsoXml xsd@(XSD (e,d)) =
  let
    (dList, el) = untwistDeps e d
  in (header <>) . runGen $ do
      for_ dList $ \(t,d) -> do
        generateIsoDatatype (Just t) d
      for_ el generateIsoRecord

toQualifier :: Int -> Maybe Int -> Text
toQualifier minOc (Just maxOc)
  | minOc == 1 && maxOc == 1 = "!"
  | minOc == 0 && maxOc == 1 = "?"
  | minOc == 1 && maxOc > minOc = "+"
  | minOc == 1 && maxOc < minOc = error "maxOccurs < minOccurs"
  | otherwise = "*"
toQualifier _ Nothing = "*"

header :: Text
header = T.unlines
  [ "module Dummy where"
  , ""
  , "import Data.THGen.XML"
  , "" ]

quote :: Text -> Text
quote t = "\"" <> t <> "\""

-- | Nothing means unknown name
typeName :: DatatypeRef -> Maybe Text
typeName (InlineComplex dt) = result
  where
    result = case dt of
      TypeComplex (ComplexType n _ _) -> n
      TypeSimple (STAtomic _ sat _)     -> Just $ simpleTypeToIsoType sat
typeName (DatatypeRef t)               = Just t

-- | Push the record to the state and return field dsl
generateIsoRecord :: Element -> GenMonad ()
generateIsoRecord (Element (_, laxName) xtype minOc maxOc) = do
  written <- get
  case xtype of
    InlineComplex dt -> generateIsoDatatype (Just laxName) dt
    DatatypeRef nam  -> if M.member nam written
      then pure ()
      else error $ "Implementation failure: "
        <> show nam <> " type never encountered before"

generateIsoDatatype
  :: (Maybe Text) -- ^ Explicitly provided name
  -> Datatype
  -> GenMonad ()
  -- ^ (type name for lookups, generated code)
generateIsoDatatype eName (TypeComplex (ComplexType iName attrs mGroupSchema)) = do
  name <- case eName of
    Just name -> pure name
    Nothing   -> case iName of
      Just name' -> pure name'
      Nothing    -> error $ "[generateIsoDatatype] no name was provided"
  case mGroupSchema of
    Just groupSchema -> case groupSchema of
      CTSequence elems -> do
        fields <- for elems generateIsoField
        let header = T.pack (capitalize (T.unpack $ quote name)) <> " =:= record Both"
        tellGen name Complex (pure header <> fields)
generateIsoDatatype eName (TypeSimple (STAtomic iName sat restrictions)) = do
  name <- case eName of
    Just name -> pure name
    Nothing   -> pure iName
  let
    isEnumeration (Enumeration _) = True
    fromEnumeration (Enumeration x) = x
    enums                         = Prelude.filter isEnumeration restrictions
    typeStr = simpleTypeToIsoType sat
  if Prelude.length enums > 0
  then tellGen
    typeStr
    (Simple sat)
    (generateEnum
      (quote $ T.pack $ capitalize $ T.unpack name)
      (F.concatMap fromEnumeration enums))
  else pure ()

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
lookupSimpleType
  :: DatatypeRef
  -> GenMonad (Maybe SimpleAtomicType)
lookupSimpleType (InlineComplex  _) = pure Nothing
lookupSimpleType (DatatypeRef tyName) = do
  M.lookup tyName <$> get >>= \case
    Just Complex     -> pure Nothing
    Just (Simple ty) -> pure $ Just ty
    Nothing          -> pure Nothing -- IS IT?

generateIsoField :: Element -> GenMonad Text
generateIsoField (Element (_, name) xtype minOc maxOc) = do
  let
    qualifier = toQualifier minOc maxOc
  line <- lookupSimpleType xtype >>= \case
    Just baseTypeName -> do
      pure $ quote name <> " " <> simpleTypeToIsoType baseTypeName
    Nothing           -> pure $ quote name
  pure $ "  " <> qualifier <> " " <> line

main :: IO ()
main = do
  (inFile:_) <- getArgs
  xml <- BL.readFile inFile
  let Right xsd = parseXSD def xml
  T.hPutStrLn stdout $ generateIsoXml xsd
