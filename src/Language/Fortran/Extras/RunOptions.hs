{-# LANGUAGE FlexibleInstances #-}

-- | This module is responsible for handling CLI options
module Language.Fortran.Extras.RunOptions
  ( FortranSrcRunOptions(..)
  , getFortranSrcRunOptions
  , unwrapFortranSrcOptions
  , RunOptions(..)
  , getRunOptions
  )
where

import qualified Data.ByteString.Char8         as B
import           Data.Char                      ( toLower )
import           Data.List                      ( isInfixOf )
import           Data.Semigroup                 ( (<>) )
import           Language.Fortran.Version       ( FortranVersion(..)
                                                , selectFortranVersion
                                                )
import           Options.Applicative

import           Language.Fortran.Util.Files
                                                ( flexReadFile )

-- | Holds fortran-src specific CLI options.
-- This includes the version of the parser, included files and the path
-- of the source
data FortranSrcRunOptions = FortranSrcRunOptions
    { version  :: !FortranVersion
    , includes :: ![String]
    , path     :: !String
    } deriving Show

-- | Provided version, includes and path strings, this functon maps them to
-- 'FortranSrcRunOptions'
--
-- Note that this will throw an exception on an unrecognized version string.
toFortranSrcOptions :: String -> [String] -> String -> FortranSrcRunOptions
toFortranSrcOptions verStr =
    let (Just ver) = selectFortranVersion verStr
     in FortranSrcRunOptions ver

-- | Definition of parser for 'FortranSrcRunOptions'
fortranSrcRunOptionsParser :: Parser FortranSrcRunOptions
fortranSrcRunOptionsParser =
  toFortranSrcOptions
    <$> strOption
          (  short 'v'
          <> long "fortranVersion"
          <> metavar "VERSION"
          <> help
               "Fortran version to use, format: Fortran[66/77/77l/77e/90/95/03/08]"
          )
    <*> many
          (strOption
            (short 'I' <> long "include" <> metavar "DIRECTORY" <> help
              "Directory to include files from"
            )
          )
    <*> argument str (metavar "PATH")

-- | Given description and header, decorate fortran-src options parser with
-- info details
fortranSrcRunOptionsInfo :: String -> String -> ParserInfo FortranSrcRunOptions
fortranSrcRunOptionsInfo programDescription headerDescription = info
  (helper <*> fortranSrcRunOptionsParser)
  (fullDesc <> progDesc programDescription <> header headerDescription)

-- | Given description and header, execute fortran-src options parser
-- and get the 'FortranSrcRunOptions'
getFortranSrcRunOptions :: String -> String -> IO FortranSrcRunOptions
getFortranSrcRunOptions programDescription headerDescription =
  execParser $ fortranSrcRunOptionsInfo programDescription headerDescription

-- | Obtain path, contents, include dirs and 'FortranVersion'
-- from 'FortranSrcRunOptions'
unwrapFortranSrcOptions
  :: FortranSrcRunOptions -> IO (String, B.ByteString, [String], FortranVersion)
unwrapFortranSrcOptions options = do
  let p = path options
  c <- flexReadFile p
  let i = includes options
      v = version options
  return (p, c, i, v)

-- | Holds 'FortranSrcRunOptions' and additional tool specific CLI options
data RunOptions a = RunOptions
    { fortranSrcOpts :: FortranSrcRunOptions
    , toolOpts :: a
    } deriving Show

-- | Given decription, header and tool options parser, decorate options
-- with info details
runOptionsInfo :: String -> String -> Parser a -> ParserInfo (RunOptions a)
runOptionsInfo programDescription headerDescription toolOptsParser = info
  (helper <*> liftA2 RunOptions fortranSrcRunOptionsParser toolOptsParser)
  (fullDesc <> progDesc programDescription <> header headerDescription)

-- | Given description, header and tool options parser, execute options parser
-- and get the 'RunOptions'
getRunOptions :: String -> String -> Parser a -> IO (RunOptions a)
getRunOptions programDescription headerDescription toolOptsParser =
  execParser
    $ runOptionsInfo programDescription headerDescription toolOptsParser
