
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
  references (Xsd.UnionType rs _) = referencesList rs

instance References Xsd.SimpleRestriction where
  references r = references (Xsd.simpleRestrictionBase r)

instance References Xsd.ComplexRestriction where
  references r = [Xsd.complexRestrictionBase r]

instance References t => References (Xsd.RefOr t) where
  references (Xsd.Ref r) = [r]
  references (Xsd.Inline t) = references t

instance References Xsd.ComplexType where
  references t = references (Xsd.complexContent t)

instance References Xsd.Content where
  references (Xsd.ContentComplex c) = references c
  references (Xsd.ContentSimple c) = references c
  references (Xsd.ContentPlain c) = references c

instance References Xsd.PlainContent where
  references t = referencesList (Xsd.plainContentAttributes t)
    ++ references (Xsd.plainContentModel t)

instance References Xsd.SimpleContent where
  references _ = []

instance References Xsd.ComplexContent where
  references (Xsd.ComplexContentExtension e) = references e
  references (Xsd.ComplexContentRestriction r) = references r

instance References Xsd.ComplexExtension where
  -- Note that we don't include extension base here
  -- becase we don't depend on it. And actaully we generate extensions before
  -- the base because we need to generate the ADT with all the extensions
  -- at this point.
  references e = referencesList (Xsd.complexExtensionAttributes e)
    ++ references (Xsd.complexExtensionModel e)

instance References Xsd.Attribute where
  references (Xsd.RefAttribute a) = references a
  references (Xsd.InlineAttribute a) = references a

instance References Xsd.AttributeRef where
  references a = [Xsd.attributeRefRef a]

instance References Xsd.AttributeInline where
  references = references . Xsd.attributeInlineType

instance References Xsd.ModelGroup where
  references (Xsd.Sequence elements) = referencesList elements
  references (Xsd.Choice elements) = referencesList elements
  references (Xsd.All elements) = referencesList elements

instance References Xsd.Element where
  references = references . Xsd.elementType
