-- | Aeson instances for definitions used later in processing (e.g. analyses).

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-} -- required due to instance design

module Language.Fortran.Extras.JSON.Analysis() where

import Language.Fortran.Extras.JSON.Helpers
import Data.Aeson
import Language.Fortran.Common.Array

instance ToJSON a => ToJSON (Dim a) where
    toJSON = gtj $ jcProdDrop "dim"

instance (ToJSON a, ToJSON (t a), ToJSON (t (Dim a))) => ToJSON (Dims t a) where
    toJSON = gtj $ jcSumDrop "Dims"
