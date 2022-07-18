module Main where

import qualified Language.Fortran.Extras.CLI.Serialize as Serialize
import qualified Options.Applicative as OA
import Control.Monad.IO.Class

data Cmd
  = CmdSerialize Serialize.Cfg
    deriving (Eq, Show)

main :: IO ()
main = execParserWithDefaults desc pCmd >>= \case
  CmdSerialize cfg -> Serialize.run cfg >>= \case
    Right ()  -> return ()
    Left  err -> print err
  where
    desc = "fortran-src extra tools"

pCmd :: OA.Parser Cmd
pCmd = OA.hsubparser $
       Serialize.cmd "serialize" "Convert between Fortran source and serialized forms" (CmdSerialize <$> Serialize.pCfg)

--------------------------------------------------------------------------------

-- | Execute a 'Parser' with decent defaults.
execParserWithDefaults :: MonadIO m => String -> OA.Parser a -> m a
execParserWithDefaults desc p = liftIO $ OA.customExecParser
    (OA.prefs $ OA.showHelpOnError)
    (OA.info (OA.helper <*> p) (OA.progDesc desc))
