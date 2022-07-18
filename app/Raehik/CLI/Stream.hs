{-# LANGUAGE AllowAmbiguousTypes, TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DerivingVia #-}

-- | Common convenience definitions I use for filesystem data I/O.
module Raehik.CLI.Stream where

import GHC.Generics ( Generic )
import Data.Data ( Typeable, Data )
import GHC.TypeLits ( Symbol, KnownSymbol, symbolVal' )
import GHC.Exts ( proxy#, Proxy# )

import Options.Applicative

import Control.Monad.IO.Class
import qualified Data.Char as Char
import qualified Data.ByteString as B

data Stream (d :: Direction) (s :: Symbol)
  = Path' (Path d s)
  | Std
    deriving stock (Generic, Typeable, Data, Show, Eq)

newtype Path (d :: Direction) (s :: Symbol)
  = Path { unPath :: FilePath }
    deriving stock (Generic, Typeable, Data)
    deriving (Show, Eq) via FilePath

data Direction = In | Out
    deriving stock (Generic, Typeable, Data, Show, Eq)

-- | Either a positional filepath, or standalone @--stdin@ switch.
pStreamIn :: forall s. KnownSymbol s => Parser (Stream 'In s)
pStreamIn = (Path' <$> pPathIn) <|> pStdinOpt
  where
    pStdinOpt = flag' Std $  long "stdin"
                          <> help ("Get "<>sym @s<>" from stdin")

-- | Either an @--out-file X@ option, or default to stdout.
pStreamOut :: forall s. KnownSymbol s => Parser (Stream 'Out s)
pStreamOut = (Path' <$> pPathOut) <|> pure Std
  where pPathOut = Path <$> strOption (modFileOut (sym @s))

-- | Positional filepath.
pPathIn :: forall s. KnownSymbol s => Parser (Path 'In s)
pPathIn = Path <$> strArgument (modFileIn (sym @s))

--------------------------------------------------------------------------------

-- | Generate a base 'Mod' for a file type using the given descriptive
--   name (the "type" of input, e.g. file format) and the given direction.
modFile :: HasMetavar f => String -> String -> Mod f a
modFile dir desc =  metavar "FILE" <> help (dir<>" "<>desc)

modFileIn :: HasMetavar f => String -> Mod f a
modFileIn = modFile "Input"

modFileOut :: (HasMetavar f, HasName f) => String -> Mod f a
modFileOut s = modFile "Output" s <> long "out-file" <> short 'o'

metavarify :: String -> String
metavarify = map $ Char.toUpper . spaceToUnderscore
  where spaceToUnderscore = \case ' ' -> '_'; ch -> ch

--------------------------------------------------------------------------------

-- | More succint 'symbolVal' via type application.
sym :: forall s. KnownSymbol s => String
sym = symbolVal' (proxy# :: Proxy# s)

--------------------------------------------------------------------------------

readStream :: forall s m. MonadIO m => Stream 'In s -> m B.ByteString
readStream = liftIO . \case Std             -> B.getContents
                            Path' (Path fp) -> B.readFile fp

writeStream :: forall s m. MonadIO m => Stream 'Out s -> B.ByteString -> m ()
writeStream s bs = liftIO $ case s of Std             -> B.putStr bs
                                      Path' (Path fp) -> B.writeFile fp bs

-- | Returns @<stdin>@ for 'Std'.
inStreamFileName :: Stream 'In s -> FilePath
inStreamFileName = \case Std             -> "<stdin>"
                         Path' (Path fp) -> fp

-- | Returns @<stdout>@ for 'Std'.
outStreamFileName :: Stream 'Out s -> FilePath
outStreamFileName = \case Std             -> "<stdout>"
                          Path' (Path fp) -> fp
