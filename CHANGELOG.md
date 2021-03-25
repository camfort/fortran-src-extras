## 0.1.0
Initial release, code adapted from a Bloomberg internal package.

### Major changes from original internal package
  * Base module name moved from `Bloomberg.FortranSrcTools.Common` to
    `Language.Fortran.Extras`.
  * Use `Language.Fortran.Util.Position.getSpan` instead of `extractExpSrcSpan`,
    `extractBlockSrcSpan`. AST nodes storing location info are instances of the
    `Spanned` typeclass which allows easy `SrcSpan` extraction.
  * `Encoding.pprint` now `Encoding.pprint77l`
    * TODO: either remove or add a few more (see if CamFort, fortran-src would
      benefit
  * Rewriter has moved: `Language.Fortran.Rewriter`
