-- | This module exposes functions obtaining both explicitly versioned
-- and implicitly versioned analyses of source code.
module Language.Fortran.Extras.Analysis
  ( versionedExpandedProgramAnalysis
  , versionedProgramAnalysis
  , versionedProgramAnalysisWithMods
  , programAnalysis
  , programAnalysisWithMods
  )
where

import qualified Data.ByteString.Char8         as B
import           Language.Fortran.AST           ( A0
                                                , ProgramFile
                                                )
import           Language.Fortran.Analysis      ( Analysis
                                                , initAnalysis
                                                )
import           Language.Fortran.Version       ( FortranVersion(..) )
import           Language.Fortran.Util.ModFile  ( combinedTypeEnv
                                                , ModFiles
                                                )

import           Language.Fortran.Extras.ProgramFile
                                                ( programFile
                                                , versionedProgramFile
                                                , versionedExpandedProgramFile
                                                )

-- | Obtain the analysis of the 'ProgramFile'.
programAnalysis' :: ProgramFile A0 -> ProgramFile (Analysis A0)
programAnalysis' = undefined

-- | Obtain the analysis of the 'ProgramFile' and modules.
programAnalysisWithMods'
  :: ModFiles -> ProgramFile A0 -> ProgramFile (Analysis A0)
programAnalysisWithMods' = undefined

-- | Obtain the analysis of source code with imports expanded using
-- a specific version of the parser.
versionedExpandedProgramAnalysis
  :: FortranVersion
  -> [String]
  -> String
  -> B.ByteString
  -> IO (ProgramFile (Analysis A0))
versionedExpandedProgramAnalysis version importDirs path contents = do
  pf <- versionedExpandedProgramFile version importDirs path contents
  return $ programAnalysis' pf

-- | Obtain the analysis of source code using an explicit version of
-- the parser.
versionedProgramAnalysis
  :: FortranVersion -> String -> B.ByteString -> ProgramFile (Analysis A0)
versionedProgramAnalysis version path contents =
  programAnalysis' $ versionedProgramFile version path contents

-- | Obtain the analysis of source code and module files using an
-- explicit version of the parser.
versionedProgramAnalysisWithMods
  :: FortranVersion
  -> ModFiles
  -> String
  -> B.ByteString
  -> ProgramFile (Analysis A0)
versionedProgramAnalysisWithMods version mods path contents =
  programAnalysisWithMods' mods $ versionedProgramFile version path contents

-- | Obtain the analysis of source code using an implicit version of
-- the parser.
programAnalysis :: String -> B.ByteString -> ProgramFile (Analysis A0)
programAnalysis path contents = programAnalysis' $ programFile path contents

-- | Obtain the analysis of source code and module files using an
-- implicit version of the parser.
programAnalysisWithMods
  :: ModFiles -> String -> B.ByteString -> ProgramFile (Analysis A0)
programAnalysisWithMods mods path contents =
  programAnalysisWithMods' mods $ programFile path contents
