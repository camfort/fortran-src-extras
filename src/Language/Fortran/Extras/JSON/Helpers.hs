{-# LANGUAGE OverloadedStrings #-}

{- | Helpers for defining Aeson instances for fortran-src types.

To work around dependency awkwardness, we have to write an unusual concrete
context @'ToJSON' 'SrcSpan'@. It seems to work fine.
-}

module Language.Fortran.Extras.JSON.Helpers
  ( toJSONAnnoMerge
  , toJSONAnnoTaggedObj
  , jcProd, jcProdDrop
  , jcSum, jcSumDrop
  , jcEnum, jcEnumDrop
  , tja, gtj, gte
  ) where

import Data.Aeson hiding ( Value )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Language.Fortran.Util.Position ( SrcSpan )
import Data.Text ( Text )

import GHC.Generics ( Generic, Rep )

-- | Shortcut for writing a 'toJSON' definition for a fortran-src AST node type,
--   intended to be used for sum types.
--
-- Flat/concise version which merges all keys into the same object.
toJSONAnnoMerge
    :: (ToJSON a, ToJSON SrcSpan)
    => Text -> a -> SrcSpan -> [Aeson.Pair] -> Aeson.Value
toJSONAnnoMerge t a ss m = object $
  [ "anno"     .= a
  , "span"     .= ss
  , "tag"      .= t ] <> m

-- | Shortcut for writing a 'toJSON' definition for a fortran-src AST node type,
--   intended to be used for sum types.
--
-- "Safe" version which approximates Aeson's default 'Data.Aeson.TaggedObject'
-- sum encoding strategy, but with two extra fields extracted out.
toJSONAnnoTaggedObj
    :: (ToJSON a, ToJSON SrcSpan)
    => Text -> a -> SrcSpan -> [Aeson.Pair] -> Aeson.Value
toJSONAnnoTaggedObj t a ss m = object
  [ "anno"     .= a
  , "span"     .= ss
  , "tag"      .= t
  , "contents" .= object m ]

-- | Shortcut for selected fortran-src AST node type 'toJSON' strategy.
tja :: (ToJSON a, ToJSON SrcSpan)
    => Text -> a -> SrcSpan -> [Aeson.Pair] -> Aeson.Value
tja = toJSONAnnoMerge

-- | Base Aeson generic deriver config for product types.
jcProd :: (String -> String) -> Aeson.Options
jcProd f = Aeson.defaultOptions
  { Aeson.rejectUnknownFields = True
  , Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . f
  }

jcProdDrop :: String -> Aeson.Options
jcProdDrop x = jcProd (drop (length x))

-- | Base Aeson generic deriver config for sum types.
jcSum :: (String -> String) -> String -> String -> Aeson.Options
jcSum f tag contents = Aeson.defaultOptions
  { Aeson.rejectUnknownFields = True
  , Aeson.constructorTagModifier = Aeson.camelTo2 '_' . f
  , Aeson.sumEncoding = Aeson.TaggedObject
    { Aeson.tagFieldName = tag
    , Aeson.contentsFieldName = contents
    }
  }

jcSumDrop :: String -> Aeson.Options
jcSumDrop x = jcSum (drop (length x)) "tag" "contents"

-- | Base Aeson generic deriver config for enum types (no fields in any cons).
jcEnum :: (String -> String) -> Aeson.Options
jcEnum f = Aeson.defaultOptions
  { Aeson.rejectUnknownFields = True
  , Aeson.constructorTagModifier = Aeson.camelTo2 '_' . f
  }

jcEnumDrop :: String -> Aeson.Options
jcEnumDrop = jcEnum . drop . length

-- | Shortcut for common function 'genericToJSON'
gtj :: (Generic a, Aeson.GToJSON' Aeson.Value Aeson.Zero (Rep a)) => Aeson.Options -> a -> Aeson.Value
gtj = genericToJSON

-- | Shortcut for common function 'genericToEncoding'
gte :: (Generic a, Aeson.GToJSON' Aeson.Encoding Aeson.Zero (Rep a)) => Aeson.Options -> a -> Aeson.Encoding
gte = genericToEncoding
