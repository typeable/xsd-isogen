{-# LANGUAGE OverloadedStrings #-}

module BuiltInTypes
( builtInTypes
)
where

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Xsd

builtInTypes :: Map Xsd.QName Text
builtInTypes = Map.fromList . map qualify $
  [ ("anyURI", "Text")
  , ("base64Binary", "ByteString")
  , ("boolean", "Bool")
  , ("date", "Date")
  , ("dateTime", "Text")
  , ("dateTimeStamp", "Text")
  , ("decimal", "Double")
  , ("integer", "Integer")
  , ("long", "Integer")
  , ("int", "Integer")
  , ("short", "Integer")
  , ("byte", "Integer")
  , ("nonNegativeInteger", "Integer")
  , ("positiveInteger", "Integer")
  , ("unsignedLong", "Integer")
  , ("unsignedInt", "Integer")
  , ("unsignedByte", "Integer")
  , ("nonPositiveInteger", "Integer")
  , ("negativeInteger", "Integer")
  , ("float", "Float")
  , ("double", "Double")
  , ("gDay", "Day")
  , ("gMonth", "Text")
  , ("gMonthDay", "Text")
  , ("gYear", "Text")
  , ("gYearMonth", "Text")
  , ("hexBinary", "Text")
  , ("NOTATION", "Text")
  , ("QName", "Text")
  , ("string", "Text")
  , ("normalizedString", "Text")
  , ("token", "Text")
  , ("language", "Text")
  , ("NCName", "Text")
  , ("Entity", "Text")
  , ("ID", "Text")
  , ("IDREF", "Text")
  , ("NMToken", "Text")
  , ("time", "Text")
  , ("duration", "Text")
  ]
  where
  qualify (n, t) =
    ( Xsd.QName (Just (Xsd.Namespace Xsd.schemaNamespace)) n, t)
