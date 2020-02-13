{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gen
( GenState(..)
, Gen
, runGen
, TypeName(..)
, getMode
, writeCode
, warn
, genError
, resolveType
, resolveTypeName
, knownTypeName
, extensions
, makeTypeName
, makeFieldName
, makeEnumName
)
where

import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import qualified Xsd

import Options (GenType(..))

-- | Generator state
data GenState = GenState
  { gsTypes :: Map Xsd.QName Xsd.Type
    -- ^ All global types
  , gsExtensions :: Map Xsd.QName [(Xsd.QName, Xsd.Type)]
    -- ^ Lift of extension of the specified type
  , gsKnownTypes :: Map Xsd.QName TypeName
    -- ^ Global types that were already processed and their generated names
  , gsBoundTypeName :: Set Text
    -- ^ prefixed names that were already used
  , gsBoundFields :: Set Text
    -- ^ field names that were already used
  , gsMode :: GenType
  , gsCode :: [Text]
  , gsWarnings :: [Text]
  }

-- | Generator monad
newtype Gen a = Gen
  { fromGen :: StateT GenState (Except String) a
  }
  deriving (Functor, Applicative, Monad)

-- | Run generator
runGen :: GenState -> Gen a -> Either String GenState
runGen s m = runExcept $ execStateT (fromGen m) s

getMode :: Gen GenType
getMode = Gen $ gets gsMode

-- | Add lines to the output
writeCode :: [Text] -> Gen ()
writeCode code = Gen $ modify' $ \s -> s
  { gsCode = reverse code ++ gsCode s
  }

-- | Emit warning
warn :: Text -> Gen ()
warn msg = Gen $ modify' $ \s -> s
  { gsWarnings = ("WARNING: " <> msg) : gsWarnings s
  }

-- | Usually we prefix types with "Xml", but for records we need plain name
--
-- Note that built-in types doesn't have prefix, so prefixed name is
-- the same as the plain one.
data TypeName = TypeName
  { tnName :: Text
    -- ^ plain name
  , tnPrefixed :: Text
    -- ^ name with "Xml" prefix
  , tnQName :: Xsd.QName
    -- ^ the original name
  }

-- | Allocate new type name for given qualified name
--
-- It checks for ambiguous type names and fields
makeTypeName :: Xsd.QName -> Xsd.Type -> Gen TypeName
makeTypeName name tp = go Nothing
  where
  go suffix = do
    let
      typeName = capitalize (Xsd.qnName name)
        <> maybe "" (Text.pack . show) suffix
      prefixed = "Xml" <> typeName
      next = case suffix of
        Nothing -> Just (1 :: Int)
        Just i -> Just (succ i)
    bound <- Gen $ gets (Set.member prefixed . gsBoundTypeName)
    duplicateFields <- hasDuplicateFields typeName tp
    if bound || duplicateFields
      then go next
      else do
        let
          tn = TypeName
            { tnName = typeName
            , tnPrefixed = prefixed
            , tnQName = name
            }
        Gen $ modify' $ \s -> s
          { gsKnownTypes = Map.insert name tn (gsKnownTypes s)
          , gsBoundTypeName = Set.insert prefixed (gsBoundTypeName s)
          }
        return tn

makeFieldName :: TypeName -> Xsd.QName -> Gen Text
makeFieldName typeName name = do
  -- here we remember that the field name is bound, so that
  -- hasDuplicateFields can check for ambiguous fields
  let prefixed = makePrefixedField (tnName typeName) name
  Gen $ modify' $ \s -> s
    { gsBoundFields = Set.insert prefixed (gsBoundFields s)
    }
  return (qualifier <> Xsd.qnName name)
  where
  qualifier = maybe ""
    (\(Xsd.Namespace ns) -> "{" <> ns <> "}")
    (Xsd.qnNamespace name)

-- | Make value for enum
--
-- We may need to handle duplicate enum values here
makeEnumName :: Text -> Gen Text
makeEnumName = return . capitalize

-- | Check whether field name is ambiguous
hasDuplicateFields :: Text -> Xsd.Type -> Gen Bool
hasDuplicateFields typeName tp = do
  bound <- forM (allFields tp) $ \f -> do
    let prefixed = makePrefixedField typeName f
    Gen $ gets (Set.member prefixed . gsBoundFields)
  return (or bound)

-- | Get all fields from the type
allFields :: Xsd.Type -> [Xsd.QName]
allFields (Xsd.TypeSimple _) = []
allFields (Xsd.TypeComplex t) =
  case Xsd.complexContent t of
    Xsd.ContentPlain c ->
      case Xsd.plainContentModel c of
        Just (Xsd.Sequence es) -> mapMaybe elementName es
        _ -> []
    Xsd.ContentSimple _ -> []
    Xsd.ContentComplex (Xsd.ComplexContentExtension e) ->
      case Xsd.complexExtensionModel e of
        Just (Xsd.Sequence es) -> mapMaybe elementName es
        _ -> []
    Xsd.ContentComplex Xsd.ComplexContentRestriction -> []
  where
  elementName = Xsd.refOr
    (const Nothing)
    (Just . Xsd.elementName)

-- | Prefix field name with based on the type name
makePrefixedField :: Text -> Xsd.QName -> Text
makePrefixedField typeName field = prefix <> Xsd.qnName field
  where
  prefix
    = Text.map Char.toLower
    . Text.filter (\c -> Char.isUpper c || Char.isDigit c)
    $ typeName

-- | Get generated name for the type name
--
-- Returns Nothing if the type was not generated yet
knownTypeName :: Xsd.QName -> Gen (Maybe TypeName)
knownTypeName name = Gen $ gets (Map.lookup name . gsKnownTypes)

extensions :: Xsd.QName -> Gen [(Xsd.QName, Xsd.Type)]
extensions base = Gen $ gets (Map.findWithDefault [] base . gsExtensions)

-- | Returns type for the reference
resolveType :: Xsd.QName -> Gen Xsd.Type
resolveType name = do
  mt <- Gen $ gets (Map.lookup name . gsTypes)
  case mt of
    Just t -> return t
    Nothing -> genError ("unknown reference to " <> show name)

-- | Returns type type for the reference
--
-- It should already be generated
resolveTypeName :: Xsd.QName -> Gen TypeName
resolveTypeName name = knownTypeName name
  >>= maybe (genError ("unresolved type: " <> show name)) return

-- | Fail the generator
genError :: String -> Gen a
genError = Gen . lift . throwE

capitalize :: Text -> Text
capitalize t
  | Text.null t  = t
  | otherwise = Text.singleton (Char.toUpper (Text.head t)) <> Text.tail t
