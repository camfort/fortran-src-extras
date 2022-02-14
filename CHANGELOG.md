## 0.3.0 (Unreleased)
  * Remove `Language.Fortran.Extras.ModFiles`. The functions are available
    in fortran-src 0.9.0 (or the first version after 0.8.0). `decodeModFiles` is
    renamed to `Language.Fortran.Util.ModFile.decodeModFiles'`.

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
