{-# LANGUAGE DataKinds #-}

-- | Fortran serializer CLI app.
module Language.Fortran.Extras.CLI.Serialize where

import Language.Fortran.Extras.JSON()
import Control.Monad.IO.Class
import qualified Options.Applicative as OA
import qualified Language.Fortran.Parser as F.Parser
import qualified Language.Fortran.Parser.Monad as F.Parser -- TODO moved to Parser in next version
import Language.Fortran.Version
import qualified Language.Fortran.Util.ModFile as F.ModFile
import qualified Raehik.CLI.Stream as CLI
import qualified Data.Char as Char
import qualified System.Exit as System

import qualified Data.Yaml as Yaml
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL

data Cfg = Cfg
  { cfgDirection       :: Direction
  , cfgFormat          :: Format
  , cfgVersion         :: FortranVersion
  , cfgIncludeHandling :: IncludeHandling
  , cfgIncludeDirs     :: [FilePath]
  } deriving (Eq, Show)

pCfg :: OA.Parser Cfg
pCfg = Cfg <$> pDirection
           <*> pFormat
           <*> pFVersion
           <*> pIncludeHandling
           <*> OA.many pIncludeDir

pIncludeDir :: OA.Parser FilePath
pIncludeDir = OA.strOption $  OA.long "include-dir"
                           <> OA.short 'i'
                           <> OA.help "Directory to search for include files in"

pFVersion :: OA.Parser FortranVersion
pFVersion =
    OA.option (OA.maybeReader selectFortranVersion) $
           OA.long "version"
        <> OA.short 'v'
        <> OA.help "Fortran version"
        <> OA.metavar "FORTRAN_VER"

-- | How to handle includes.
data IncludeHandling = DefaultIncludes | InlineIncludes
    deriving (Eq, Show)

pIncludeHandling :: OA.Parser IncludeHandling
pIncludeHandling = OA.flag DefaultIncludes InlineIncludes $
       OA.long "inline-includes"
    <> OA.help "Process (expand) include statement inline"

-- | Serialization format.
data Format = FmtJson | FmtYaml
    deriving (Eq, Show)

pFormat :: OA.Parser Format
pFormat = OA.option (OA.maybeReader go) $ OA.long "format" <> OA.short 't' <> OA.help "Serialization format (allowed: json, yaml)" <> OA.metavar "FORMAT"
  where
    go x =
        case map Char.toLower x of
          "json" -> Just FmtJson
          "yaml" -> Just FmtYaml
          _      -> Nothing

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

pDirection :: OA.Parser Direction
pDirection = OA.hsubparser $
       cmd "encode" "Serialize Fortran source to the requested format" pDirectionEncode
    <> cmd "decode" "Process serialized Fortran into Fortran source"   pDirectionDecode

pDirectionEncode, pDirectionDecode :: OA.Parser Direction
pDirectionEncode = DirEncode <$> CLI.pStreamIn <*> CLI.pStreamOut
pDirectionDecode = DirDecode <$> CLI.pStreamIn <*> CLI.pStreamOut

runEncodeAndPrint :: (MonadIO m, Aeson.ToJSON a) => Format -> CLI.Stream 'CLI.Out _s -> a -> m ()
runEncodeAndPrint fmt sOut a = CLI.writeStream sOut textBsOut
  where textBsOut = case fmt of FmtJson -> BL.toStrict $ Aeson.encode a
                                FmtYaml -> Yaml.encode a

run :: MonadIO m => Cfg -> m (Either F.Parser.ParseErrorSimple ())
run cfg = do
    case cfgDirection cfg of
      DirEncode sIn sOut -> do
        bsIn <- CLI.readStream sIn
        case cfgIncludeHandling cfg of
          DefaultIncludes ->
            let parser = F.Parser.byVer (cfgVersion cfg)
             in case parser (CLI.inStreamFileName sIn) bsIn of
                  Left  e   -> return $ Left e
                  Right src -> do
                    runEncodeAndPrint (cfgFormat cfg) sOut src
                    return $ Right ()
          InlineIncludes ->
            case cfgVersion cfg of
              Fortran77Extended -> do
                let parserM = F.Parser.f77lIncludes ("." : cfgIncludeDirs cfg) F.ModFile.emptyModFiles
                src <- liftIO $ parserM (CLI.inStreamFileName sIn) bsIn
                runEncodeAndPrint (cfgFormat cfg) sOut src
                return $ Right ()
              v -> err 2 $  "--inline-includes only works with"
                         <> " version Fortran77Extended"
                         <> " (got: "<>show v<>")"
      DirDecode _sIn _sOut -> do
        err 1 "converting from serialized Fortran to Fortran source not yet implemented"

--------------------------------------------------------------------------------
-- IO helpers

err :: MonadIO m => Int -> String -> m a
err ec msg = liftIO $ do putStrLn msg
                         System.exitWith $ System.ExitFailure ec

exit :: MonadIO m => Int -> m a
exit n = liftIO $ System.exitWith $ System.ExitFailure n

--------------------------------------------------------------------------------
-- CLI helpers

-- | Shorthand for defining a CLI command.
cmd :: String -> String -> OA.Parser a -> OA.Mod OA.CommandFields a
cmd name desc p = OA.command name (OA.info p (OA.progDesc desc))
