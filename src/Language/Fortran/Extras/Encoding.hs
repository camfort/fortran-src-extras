{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Utils and Aeson orphan instances for common types in the AST.
module Language.Fortran.Extras.Encoding where

import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                , encode
                                                )
import           Language.Fortran.AST           ( BaseType )
import           Language.Fortran.Analysis.SemanticTypes
                                                ( CharacterLen
                                                , SemType
                                                )
import           Language.Fortran.Version       ( FortranVersion(..) )
import           Language.Fortran.PrettyPrint   ( IndentablePretty
                                                , pprintAndRender
                                                )
import           Language.Fortran.Util.Position ( Position
                                                , SrcSpan
                                                )
import           Data.ByteString.Lazy           ( ByteString )

-- | Provide a wrapper for the 'Data.Aeson.encode' function to allow
-- indirect use in modules importing
-- 'Language.Fortran.Extras.Encoding'.
commonEncode :: ToJSON a => a -> ByteString
commonEncode = encode

instance ToJSON Position
instance FromJSON Position

instance ToJSON SrcSpan
instance FromJSON SrcSpan

instance ToJSON CharacterLen
instance FromJSON CharacterLen

instance ToJSON SemType
instance FromJSON SemType

instance ToJSON BaseType
instance FromJSON BaseType

-- | Render some AST element to a 'String' using F77 legacy mode.
pprint77l :: IndentablePretty a => a -> String
pprint77l s = pprintAndRender Fortran77Legacy s Nothing
