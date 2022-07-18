{-# LANGUAGE DataKinds #-}

-- | Fortran serializer CLI app.
module Language.Fortran.Extras.CLI.Serialize where

import Language.Fortran.Extras.JSON()
import Control.Monad.IO.Class
import qualified Options.Applicative as OA
import qualified Language.Fortran.Parser as F.Parser
import qualified Language.Fortran.Parser.Monad as F.Parser -- TODO moved to Parser in next version
import Language.Fortran.Version
import qualified Raehik.CLI.Stream as CLI
import qualified Data.Char as Char
import qualified System.Exit as System

import qualified Data.Yaml as Yaml
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL

data Cfg = Cfg
  { cfgDirection :: Direction
  , cfgFormat    :: Format
  , cfgVersion   :: FortranVersion
  } deriving (Eq, Show)

-- | Serialization format.
data Format = FmtJson | FmtYaml
    deriving (Eq, Show)

-- | Coding direction.
--
-- _Encode_ means to consume Fortran source and produce serialized Fortran.
-- _Decode_ means to consume serialized Fortran and produce Fortran source.
data Direction
  = DirEncode (CLI.Stream 'CLI.In  "Fortran source")
              (CLI.Stream 'CLI.Out "serialized Fortran")
  | DirDecode (CLI.Stream 'CLI.In  "serialized Fortran")
              (CLI.Stream 'CLI.Out "Fortran source")
    deriving (Eq, Show)

pCfg :: OA.Parser Cfg
pCfg = Cfg <$> pDirection <*> pFormat <*> pFVersion

pFVersion :: OA.Parser FortranVersion
pFVersion =
    OA.option (OA.maybeReader selectFortranVersion) $
           OA.long "version"
        <> OA.short 'v'
        <> OA.help "Fortran version"
        <> OA.metavar "FORTRAN_VER"

pFormat :: OA.Parser Format
pFormat = OA.option (OA.maybeReader go) $ OA.long "format" <> OA.short 't' <> OA.help "Serialization format (allowed: json, yaml)" <> OA.metavar "FORMAT"
  where
    go x =
        case map Char.toLower x of
          "json" -> Just FmtJson
          "yaml" -> Just FmtYaml
          _      -> Nothing

pDirection :: OA.Parser Direction
pDirection = OA.hsubparser $
       cmd "encode" "Serialize Fortran source to the requested format" pDirectionEncode
    <> cmd "decode" "Process serialized Fortran into Fortran source"   pDirectionDecode

pDirectionEncode, pDirectionDecode :: OA.Parser Direction
pDirectionEncode = DirEncode <$> CLI.pStreamIn <*> CLI.pStreamOut
pDirectionDecode = DirDecode <$> CLI.pStreamIn <*> CLI.pStreamOut

run :: MonadIO m => Cfg -> m (Either F.Parser.ParseErrorSimple ())
run cfg = do
    case cfgDirection cfg of
      DirEncode sIn sOut -> do
        bsIn <- CLI.readStream sIn
        case (F.Parser.byVer (cfgVersion cfg)) (CLI.inStreamFileName sIn) bsIn of
          Left  e   -> return $ Left e
          Right src -> do
            let textBsOut = case cfgFormat cfg of
                       FmtJson -> BL.toStrict $ Aeson.encode src
                       FmtYaml -> Yaml.encode src
            CLI.writeStream sOut textBsOut
            return $ Right ()
      DirDecode _sIn _sOut-> do
        liftIO $ putStrLn "converting from serialized Fortran to Fortran source not yet implemented"
        exit 1

--------------------------------------------------------------------------------
-- IO helpers

exit :: MonadIO m => Int -> m a
exit n = liftIO $ System.exitWith $ System.ExitFailure n

--------------------------------------------------------------------------------
-- CLI helpers

-- | Shorthand for defining a CLI command.
cmd :: String -> String -> OA.Parser a -> OA.Mod OA.CommandFields a
cmd name desc p = OA.command name (OA.info p (OA.progDesc desc))
