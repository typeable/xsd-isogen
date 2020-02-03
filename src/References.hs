
module References
( References(..)
)
where

import qualified Xsd

class References a where
  references :: a -> [Xsd.QName]
  referencesList :: [a] -> [Xsd.QName]
  referencesList = concatMap references

instance References t => References (Maybe t) where
  references Nothing = []
  references (Just t) = references t

instance References Xsd.Type where
  references (Xsd.TypeSimple t) = references t
  references (Xsd.TypeComplex t) = references t

instance References Xsd.SimpleType where
  references (Xsd.AtomicType r _) = references r
  references (Xsd.ListType r _) = references r

instance References Xsd.Restriction where
  references r = references (Xsd.restrictionBase r)

instance References t => References (Xsd.RefOr t) where
  references (Xsd.Ref r) = [r]
  references (Xsd.Inline t) = references t

instance References Xsd.ComplexType where
  references t = referencesList (Xsd.complexAttributes t)
    ++ references (Xsd.complexModelGroup t)

instance References Xsd.Attribute where
  references = references . Xsd.attrType

instance References Xsd.ModelGroup where
  references (Xsd.Sequence elements) = referencesList elements
  references (Xsd.Choice elements) = referencesList elements
  references (Xsd.All elements) = referencesList elements

instance References Xsd.Element where
  references = references . Xsd.elementType
