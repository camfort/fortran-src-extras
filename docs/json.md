# fortran-src Fortran code JSON representation
This package contains code to serialize parsed Fortran code to a highly
structured JSON document. This document intends to provide an overview of the
schema. Note that this is not a supported feature, so things may be omitted -
refer to the `Language.Fortran.AST` module in fortran-src and the
`Language.Fortran.Extra.JSON` module in fortran-src-extras for further info.

## Rationale
Due to the large amount of structure in play, it's ideal to use the types
directly by using fortran-src as a dependency in a Haskell project. For cases
where you want to use fortran-src in a non-Haskell project, we provide a rough
JSON serialization of successfully parsed Fortran code, which can be parsed and
used in any context that has JSON parsing.


