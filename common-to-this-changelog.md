  * Use `Language.Fortran.Util.Position.getSpan` instead of `extractExpSrcSpan`,
    `extractBlockSrcSpan`. AST nodes storing location info are instances of the
    `Spanned` typeclass which allows easy `SrcSpan` extraction.
  * `Encoding.pprint` now `Encoding.pprint77l`
    * TODO: either remove or add a few more (see if CamFort, fortran-src would
      benefit
  * Rewriter has moved: TODO get the new name.

Not yet done:

  * Use `universeBi` directly instead of the type-specified versions `allB`,
    `allS`, `allPUB`, `allPUS`. You may want to use TypeApplications e.g.
    `@ProgramFile` or (expr :: Type) syntax for any type errors that crop up.
    * `allB       :: Data a          => ProgramFile a -> [Block a]`
    * `universeBi :: Biplate from to => from          -> [to]`
