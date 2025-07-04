## 0.5.5 (2025-07-03)
  * bump fortran-src to ==0.16.\*

## 0.5.0 (2023-03-21)
  * bump fortran-src to ==0.14.\*

## 0.4.1 (2023-03-15)
  * fix testing from sdist package (some test data files weren't bundled)
  * bump fortran-src upper bound to 0.13

## 0.4.0 (2022-10-19)
  * Update to fortran-src 0.12.0 (bumps minimum compiler version to GHC 9.0)
  * Set upper bounds for various dependencies

## 0.3.2 (2022-08-22)
  * Update to fortran-src 0.10.1
  * Serializer: add include inlining support (F77e only)
  * Fix Aeson minimum bound

## 0.3.1 (2022-07-18)
  * Update to fortran-src 0.10.0
  * Add helpers for using Fortran 77 include parser with IO actions
    `Language.Fortran.Extras.withToolOptionsAndProgramOrBlock`
  * Add `ToJSON` instances for data types in `Language.Fortran.AST`
    * See `docs/json/schema.md` for notes on migrating from inspiration schema
  * Add fortran-src-extras executable with a command for serializing Fortran
    source into JSON (and YAML)

## 0.3.0 (2022-02-15)
  * Update to fortran-src 0.9.0
  * Remove `Language.Fortran.Extras.ModFiles`. The functions are available
    in fortran-src 0.9.0. `decodeModFiles` is renamed to
    `Language.Fortran.Util.ModFile.decodeModFiles'`.

## 0.2.0 (2021-06-30)
  * Minor changes to the documentation
  * Minor changes to provided `withProgramAnalysis` interface to include verbose mode
  * Add Aeson instances for SemType
  * Update fortran-src dependency

## 0.1.0
Initial release.

### Major changes from original package
  * Use `Language.Fortran.Util.Position.getSpan` instead of `extractExpSrcSpan`,
    `extractBlockSrcSpan`. AST nodes storing location info are instances of the
    `Spanned` typeclass which allows easy `SrcSpan` extraction.
  * `Encoding.pprint` now `Encoding.pprint77l`
  * Rewriter has moved: `Language.Fortran.Rewriter` (in fortran-src package)
