-- | Aeson instances for "small" definitions used in representing Fortran code.

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fortran.Extras.JSON.Supporting() where

import Language.Fortran.Extras.JSON.Helpers
import Language.Fortran.Extras.Util
import Data.Aeson
import Language.Fortran.Util.Position
import Language.Fortran.Version
import Language.Fortran.AST.AList

instance (ToJSON (t a), ToJSON a) => ToJSON (AList t a) where
    toJSON     = gtj $ jcProdDrop "alist"
    toEncoding = gte $ jcProdDrop "alist"
instance (ToJSON a, ToJSON (t1 a), ToJSON (t2 a)) => ToJSON (ATuple t1 t2 a) where
    toJSON     = gtj $ jcProdDrop "atuple"
    toEncoding = gte $ jcProdDrop "atuple"

instance ToJSON FortranVersion where
    toJSON     = gtj $ jcEnumDrop mempty
    toEncoding = gte $ jcEnumDrop mempty

instance ToJSON Position where
    toJSON     = String . tshow
    toEncoding = toEncoding . tshow
instance ToJSON SrcSpan where
    toJSON     = String . tshow
    toEncoding = toEncoding . tshow

{- FromJSON instances

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Void ( Void )
import Data.Text ( Text )
import Data.Functor ( void )

-- TODO better error reporting
instance FromJSON Position where
    parseJSON  = withText "position" $ \t ->
        case parseMaybe pPosition t of
          Nothing  -> fail "failed to parse position"
          Just pos -> pure pos

-- TODO better error reporting
instance FromJSON SrcSpan where
    parseJSON  = withText "SrcSpan" $ \t ->
        case parseMaybe pSrcSpan t of
          Nothing -> fail "failed to parse SrcSpan"
          Just ss -> pure ss

type Parser = Parsec Void Text

pPosition :: Parser Position
pPosition = do
    posLine'   <- L.decimal
    void $ char ':'
    posColumn' <- L.decimal
    return initPosition { posLine = posLine', posColumn = posColumn' }

pSrcSpan :: Parser SrcSpan
pSrcSpan = do
    void $ char '('
    posFrom <- pPosition
    void $ string ")-("
    void $ char ')'
    posTo   <- pPosition
    return $ SrcSpan posFrom posTo

-}
