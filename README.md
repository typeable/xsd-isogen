# xsd-isogen

Tool to generate xml-isogen descriptions from xsd files

Usage
=====

Use `xsd-isogen in.xsd Generator > out.hs` for queries.

Use `xsd-isogen in.xsd Parser > out.hs` for responses.

Use `xsd-isogen in.xsd Both > out.hs` for common definitions.

Keep in mind that name clashes are possible and should be disambiguated
manually. If you have a good idea how to handle that, feel free to create an
issue and/or PR. Module name for all auto-generated files is set to `module
Dummy where`, so it should be updated appropriately as well.
