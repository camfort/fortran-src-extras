{-# LANGUAGE TupleSections #-}

module Language.Fortran.Extras where

import           Control.Exception              ( try
                                                , SomeException
                                                )
import           Data.Data                      ( Data )
import           Data.List                      ( find
                                                )
import           Data.Maybe                     ( fromMaybe
                                                , mapMaybe
                                                )
import           Data.Generics.Uniplate.Data    ( universeBi )
import           Language.Fortran.AST           ( A0
                                                , Block
                                                , ProgramFile
                                                , Statement
                                                , ProgramUnit(..)
                                                , ProgramUnitName(..)
                                                )
import           Language.Fortran.Analysis      ( Analysis
                                                , puSrcName
                                                )
import           Language.Fortran.Version       ( FortranVersion(..) )
import           System.FilePath                ( takeExtension )
import           System.Exit                    ( ExitCode(..)
                                                , exitWith
                                                )
import           System.IO                      ( hPutStr
                                                , hPutStrLn
                                                , stderr
                                                )
import           Options.Applicative
import qualified Language.Fortran.Parser as Parser
import qualified Language.Fortran.Extras.ProgramFile
                                               as P
import qualified Language.Fortran.Extras.Analysis
                                               as A
import           Language.Fortran.Util.ModFile  ( decodeModFiles' )
import           Language.Fortran.Extras.RunOptions
                                                ( unwrapFortranSrcOptions
                                                , getFortranSrcRunOptions
                                                , getRunOptions
                                                , FortranSrcRunOptions(..)
                                                , RunOptions(..)
                                                )

-- | Get a list of all 'Block's in a 'ProgramFile'
allB :: Data a => ProgramFile a -> [Block a]
allB = universeBi

-- | Get a list of all 'Statement's in a 'ProgramFile'
allS :: Data a => ProgramFile a -> [Statement a]
allS = universeBi

-- | Get a list of all 'ProgramUnit's in a 'ProgramFile'
allPU :: Data a => ProgramFile a -> [ProgramUnit a]
allPU = universeBi

-- | Get a list of all 'Block's in a 'ProgramUnit'
allPUB :: Data a => ProgramUnit a -> [Block a]
allPUB = universeBi

-- | Get a list of all 'Statement's in a 'ProgramUnit'
allPUS :: Data a => ProgramUnit a -> [Statement a]
allPUS = universeBi

-- | Given a 'ProgramFile' find a 'ProgramUnit' with a particular 'ProgramUnitName'
findPU'
  :: Data a
  => ProgramUnitName
  -> ProgramFile (Analysis a)
  -> Maybe (ProgramUnit (Analysis a))
findPU' n = find (\pu -> puSrcName pu == n) . allPU

-- | Given a 'ProgramFile' find a 'ProgramUnit' with a particular name
findPU
  :: Data a
  => String
  -> ProgramFile (Analysis a)
  -> Maybe (ProgramUnit (Analysis a))
findPU n = findPU' $ Named n

-- | Get a 'ProgramFile' from version and path specified in 'FortranSrcRunOptions'
programFile :: FortranSrcRunOptions -> IO (ProgramFile A0)
programFile options = do
  (pfPath, pfContents, pfIncludes, fVersion) <- unwrapFortranSrcOptions options
  case fVersion of
    Fortran77Legacy ->
      P.versionedExpandedProgramFile fVersion pfIncludes pfPath pfContents
    _ -> return $ P.versionedProgramFile fVersion pfPath pfContents

incFile :: FortranSrcRunOptions -> IO [Block A0]
incFile options = do
  (pfPath, pfContents, _pfIncludes, _fVersion) <- unwrapFortranSrcOptions options
  Parser.throwIOLeft $ Parser.f77lIncludesNoTransform pfPath pfContents

-- | Get a 'ProgramFile' with 'Analysis' from version and path specified
-- in 'FortranSrcRunOptions'
programAnalysis :: FortranSrcRunOptions -> IO (ProgramFile (Analysis A0))
programAnalysis options = do
  (pfPath, pfContents, pfIncludes, fVersion) <- unwrapFortranSrcOptions options
  case fVersion of
    Fortran77Legacy ->
      A.versionedExpandedProgramAnalysis fVersion pfIncludes pfPath pfContents
    _ -> if null pfIncludes
      then return $ A.versionedProgramAnalysis fVersion pfPath pfContents
      else do
        pfMods <- decodeModFiles' pfIncludes
        return $ A.versionedProgramAnalysisWithMods fVersion
                                                    pfMods
                                                    pfPath
                                                    pfContents

-- | Parse arguments and return 'ProgramFile'
--
-- This function has the purpose of being a general entry-point for `fortran-src-tools` tools.
-- It handles parsing the common arguments and then returns a 'ProgramFile' that the tool can then use to
-- do further processing.
-- 
-- This function takes in two arguments, namely:
--
-- * A description of the program that shows up when the program is invoked incorrectly
-- @
-- $ some-fortran-tool
-- Missing: (-v|--fortranVersion VERSION) PATH
-- 
-- Usage: vars (-v|--fortranVersion VERSION) [-I|--include DIRECTORY] PATH
--   THIS IS WHERE THE DESCRIPTION GOES
-- @
--
-- * A header that is shown when the user passes the `--help` argument to the tool (note that this does
--  not show up when the program is invoked incorrectly)
-- @
-- $ some-fortran-tool --help
-- THIS IS WHERE THE HEADER GOES
-- 
-- Usage: vars (-v|--fortranVersion VERSION) [-I|--include DIRECTORY] PATH
--   THIS IS WHERE THE DESCRIPTION GOES
-- 
-- Available options:
--    -h,--help                Show this help text
--    -v,--fortranVersion VERSION
--                             Fortran version to use, format:
--                             Fortran[66/77/BigIron/77Legacy/77Extended/90]
--    -I,--include DIRECTORY   Directory to include files from
-- @
getProgramFile :: String -> String -> IO (ProgramFile A0)
getProgramFile programDescription programHeader = do
  options <- getFortranSrcRunOptions programDescription programHeader
  programFile options

-- | Parse arguments and return a 'ProgramFile' with 'Analysis'
--
-- This function takes the same arguments as 'getProgramFile', however it will return an 'Analysis' object
-- within the 'ProgramFile' monad. If any `-I DIR` arguments were specified when invoking the tool, this
-- function will ensure that any module files located in this directory are loaded and incorporated into the
-- analysis.
getProgramAnalysis :: String -> String -> IO (ProgramFile (Analysis A0))
getProgramAnalysis programDescription programHeader = do
  options <- getFortranSrcRunOptions programDescription programHeader
  programAnalysis options

-- | Helper to print out exceptions with the name of the file being processed
errorHandler :: String -> Either SomeException () -> IO ()
errorHandler filename (Left e) = do
  hPutStrLn stderr $ "Caught exception in file: " ++ filename
  hPutStr stderr . unlines . map ("    " ++) . lines $ show e
  exitWith $ ExitFailure 2
errorHandler _ (Right _) = return ()

-- | Given a program description, a program header, and a handler that
-- takes a 'ProgramFile', this function generates the 'ProgramFile'
-- and passes it to the handler, while catching any exceptions that
-- occur within either the parsing of the 'ProgramFile' itself
-- or while the handler is processing
withProgramFile :: String -> String -> (ProgramFile A0 -> IO ()) -> IO ()
withProgramFile programDescription programHeader handler = do
  options <- getFortranSrcRunOptions programDescription programHeader
  results <- try $ programFile options >>= handler
  errorHandler (path options) results

-- | Given a program description, a program header, and a handler that
-- takes a 'ProgramFile', this function generates the 'ProgramFile'
-- annotated with 'Analysis' and passes it to the handler,
-- while catching any exceptions that occur within either the parsing
-- of the 'ProgramFile' itself or while the handler is processing
withProgramAnalysis
  :: String -> String -> (ProgramFile (Analysis A0) -> IO ()) -> IO ()
withProgramAnalysis programDescription programHeader handler = do
  options <- getFortranSrcRunOptions programDescription programHeader
  results <- try $ programAnalysis options >>= handler
  errorHandler (path options) results

-- | Given a program description, a program header, a parser for
-- tool CLI options, and a handler that takes tool CLI options object,
-- and a 'ProgramFile', this function generates tool CLI options object,
-- and a 'ProgramFile' annotated with 'Analysis', and passes them to the
-- handler, while catching any exceptions that occurs during handler
-- processing
withToolOptionsAndProgramAnalysis
  :: String
  -> String
  -> Parser a
  -> (a -> ProgramFile (Analysis A0) -> IO ())
  -> IO ()
withToolOptionsAndProgramAnalysis programDescription programHeader toolOptsParser handler
  = do
    options <- getRunOptions programDescription programHeader toolOptsParser
    let (fortranSrcOptions, toolOptions) =
          (fortranSrcOpts options, toolOpts options)
    results <- try $ programAnalysis fortranSrcOptions >>= handler toolOptions
    errorHandler (path fortranSrcOptions) results

-- | Given a program description, a program header, cli options parser, and a
-- handler which takes the cli options and either an include files '[Block A0]'
-- or 'Programfile A0', run the handler on the appropriately parsed source,
-- parsing anything that has the ".inc" extension as an include
withToolOptionsAndProgramOrBlock
  :: String
  -> String
  -> Parser a
  -> (a -> Either (FilePath, [Block A0]) (ProgramFile A0) -> IO ())
  -> IO ()
withToolOptionsAndProgramOrBlock programDescription programHeader optsParser handler = do
  RunOptions srcOptions toolOptions <-
    getRunOptions programDescription programHeader optsParser
  ast <- if takeExtension (path srcOptions) `elem` [".inc", ".ins"]
    then Left . (path srcOptions, ) <$> incFile srcOptions
    else Right <$> programFile srcOptions
  handler toolOptions ast

-- | Given a 'ProgramUnit' return a pair of the name of the unit as well as the unit itself
-- only if the 'ProgramUnit' is a 'PUMain', 'PUSubroutine', or a 'PUFunction'
namedProgramUnit :: Data a => ProgramUnit a -> Maybe (String, ProgramUnit a)
namedProgramUnit pu@(PUMain _ _ mn _ _) = Just (fromMaybe "MAIN" mn, pu)
namedProgramUnit pu@(PUSubroutine _ _ _ n _ _ _) = Just (n, pu)
namedProgramUnit pu@(PUFunction _ _ _ _ n _ _ _ _) = Just (n, pu)
namedProgramUnit _ = Nothing

-- | Given a 'ProgramFile' return all of the named 'ProgramUnits' within the file, i.e.
-- 'PUMain's, 'PUSubroutine's, or 'PUFunction's
namedProgramUnits :: Data a => ProgramFile a -> [(String, ProgramUnit a)]
namedProgramUnits = mapMaybe namedProgramUnit . allPU
