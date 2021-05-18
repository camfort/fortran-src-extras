-- | This module exposes functions obtaining
-- 'Language.Fortran.AST.ProgramFile' from a valid file name.
module Language.Fortran.Extras.ProgramFile where

import qualified Data.ByteString.Char8         as B
import           Language.Fortran.AST           ( A0
                                                , ProgramFile
                                                )
import           Language.Fortran.Version       ( deduceFortranVersion
                                                , FortranVersion(..)
                                                )
import           Language.Fortran.Parser.Any    ( parserVersions
                                                )
import           Language.Fortran.ParserMonad   ( fromParseResult
                                                , fromRight
                                                )
import           Language.Fortran.Parser.Fortran77
                                                ( legacy77ParserWithIncludes )
import           System.FilePath                ( takeDirectory )

-- | Obtain a 'ProgramFile' from a specific version of the parser with include
-- statements expanded.
--
-- TODO: cover all FortranVersions, instead of just Fortran77Legacy
versionedExpandedProgramFile
  :: FortranVersion -> [String] -> String -> B.ByteString -> IO (ProgramFile A0)
versionedExpandedProgramFile version importDirs path contents = case version of
  Fortran77Legacy ->
    let parserF b s =
            fromRight
              . fromParseResult
              <$> legacy77ParserWithIncludes (takeDirectory path : importDirs) b s
    in  parserF contents path
  _ -> error ("Unsupported version: " ++ show version)

-- | Obtain a 'ProgramFile' from a specific version of the parser.
versionedProgramFile
  :: FortranVersion -> String -> B.ByteString -> ProgramFile A0
versionedProgramFile version path contents =
  let parserF = parserVersions version
      pf      = fromRight $ parserF contents path -- Parse contents of file
  in  pf

-- | Obtain a 'ProgramFile' from a parser version deduced by inspection
-- of the file extension.
--
-- For example "foo.f90" will deduce the 'Fortran90' Parser version.
programFile :: String -> B.ByteString -> ProgramFile A0
programFile path contents =
  let version = deduceFortranVersion path -- suggest version from file extension
  in  versionedProgramFile version path contents
