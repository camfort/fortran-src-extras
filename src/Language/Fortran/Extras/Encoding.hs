{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Utils and Aeson orphan instances for common types in the AST.
--
-- Partially deprecated by proper JSON serialization support.
module Language.Fortran.Extras.Encoding where

import Language.Fortran.Extras.JSON()
import Data.Aeson ( ToJSON, encode )
import Data.ByteString.Lazy ( ByteString )
import Language.Fortran.PrettyPrint ( IndentablePretty, pprintAndRender )
import Language.Fortran.Version ( FortranVersion(..) )
import Language.Fortran.Util.Position ( SrcSpan(..), Position(..) )

-- | Provide a wrapper for the 'Data.Aeson.encode' function to allow
-- indirect use in modules importing
-- 'Language.Fortran.Extras.Encoding'.
commonEncode :: ToJSON a => a -> ByteString
commonEncode = encode

-- | Render some AST element to a 'String' using F77 legacy mode.
pprint77l :: IndentablePretty a => a -> String
pprint77l s = pprintAndRender Fortran77Legacy s Nothing

getFilePath :: SrcSpan -> FilePath
getFilePath (SrcSpan p _) = posFilePath p

showFileSpan :: SrcSpan -> String
showFileSpan s@(SrcSpan p _) = posFilePath p <> "-" <> show s
