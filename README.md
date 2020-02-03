# xsd-isogen

Tool to generate xml-isogen descriptions from xsd files

Usage
=====

Use `xsd-isogen -i in.xsd -m Generator > out.hs` for queries.

Use `xsd-isogen -i in.xsd -m Parser > out.hs` for responses.

Use `xsd-isogen -i in.xsd -m Both > out.hs` for common definitions.

If there are conflicting field names in some datatypes, one of the the datatype names gets consecutive numbers starting from 2 appended to it (so that the field name prefix changes) until there are no more conflicts.

## Available options:
  - `--no-header`:
    Do not place a header at the top of the file.
  - `--header ARG`:
    A custom header to be placed at the top of the file.
  - `-m,--mode ARG`:
    Whether to generate both parsers and generators. One of: Parser, Generator, Both.
  - `-i,--input ARG`:
    Path to the input .xsd file.
  - `-n,--name ARG`:
    Specify the name of the generated module.
  - `-h,--help`:
    Show this help text
