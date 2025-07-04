-- | This module exposes functions obtaining
-- 'Language.Fortran.AST.ProgramFile' from a valid file name.
module Language.Fortran.Extras.ProgramFile where

import qualified Data.ByteString.Char8      as B
import           Language.Fortran.AST       ( A0
                                            , ProgramFile
                                            )
import           Language.Fortran.Version   ( deduceFortranVersion
                                            , FortranVersion(..)
                                            )
import qualified Language.Fortran.Parser    as Parser
import           System.FilePath            ( takeDirectory )

-- | Obtain a 'ProgramFile' from a specific version of the parser with include
-- statements expanded.
--
-- TODO: cover all FortranVersions, instead of just Fortran77Legacy
versionedExpandedProgramFile
  :: FortranVersion -> [String] -> String -> B.ByteString -> IO (ProgramFile A0)
versionedExpandedProgramFile v importDirs path =
    Parser.byVerInlineIncludes v (takeDirectory path : importDirs) [] path

-- | Obtain a 'ProgramFile' from a specific version of the parser.
versionedProgramFile
  :: FortranVersion -> String -> B.ByteString -> ProgramFile A0
versionedProgramFile v p
  = either (error . ("Parse error: " <>) . show) id
  . Parser.byVer v p

-- | Obtain a 'ProgramFile' from a parser version deduced by inspection
-- of the file extension.
--
-- For example "foo.f90" will deduce the 'Fortran90' Parser version.
programFile :: String -> B.ByteString -> ProgramFile A0
programFile path contents =
  let version = deduceFortranVersion path -- suggest version from file extension
  in  versionedProgramFile version path contents
