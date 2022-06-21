-- | Aeson instances for definitions used for representing Fortran literals.

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Fortran.Extras.JSON.Literals() where

import Language.Fortran.Extras.JSON.Helpers
import Language.Fortran.Extras.JSON.Supporting()
import Data.Aeson
import Language.Fortran.AST.Literal
import qualified Language.Fortran.AST.Literal.Boz as Boz
import Language.Fortran.AST.Literal.Boz
import qualified Language.Fortran.AST.Literal.Real as Real
import Language.Fortran.AST.Literal.Real
import Language.Fortran.AST.Literal.Complex

instance ToJSON a => ToJSON (KindParam a) where toJSON = gtj $ jcSumDrop "KindParam"

-- TODO override to reparse/print?
instance ToJSON Boz.Conforming where toJSON = gtj $ jcEnum id
instance ToJSON BozPrefix where toJSON = gtj $ jcEnumDrop "BozPrefix"
instance ToJSON Boz where toJSON = gtj $ jcProd $ drop $ length "boz"

-- TODO override to reparse/print?
instance ToJSON Real.ExponentLetter where toJSON = gtj $ jcEnumDrop "ExpLetter"
instance ToJSON Real.Exponent where toJSON = gtj $ jcProdDrop "exponent"
instance ToJSON RealLit where toJSON = gtj $ jcProdDrop "realLit"

-- TODO override to reparse/print?
instance ToJSON a => ToJSON (ComplexPart a) where toJSON = gtj $ jcSumDrop "ComplexPart"
instance ToJSON a => ToJSON (ComplexLit a) where toJSON = gtj $ jcProdDrop "complexLit"
