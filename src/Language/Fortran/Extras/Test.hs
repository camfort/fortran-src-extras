{-# LANGUAGE OverloadedStrings #-}
module Language.Fortran.Extras.Test where

import qualified Data.ByteString.Lazy.Char8    as BC
import           Data.Algorithm.Diff            ( getDiff )
import           Data.Algorithm.DiffOutput      ( ppDiff )
import           Language.Fortran.Analysis      ( Analysis )
import           Language.Fortran.AST           ( A0
                                                , ProgramFile
                                                )
import           Language.Fortran.Version       ( FortranVersion(..) )

import           Language.Fortran.Extras.Analysis
                                                ( versionedExpandedProgramAnalysis
                                                )
import           Language.Fortran.Extras.ProgramFile
                                                ( versionedExpandedProgramFile )
import           Language.Fortran.Util.Files
                                                ( flexReadFile )

getTestProgramFile :: String -> IO (ProgramFile A0)
getTestProgramFile = getTestProgramIncludesByVer Fortran77Legacy []

getTestProgramFileIncludes :: String -> [String] -> IO (ProgramFile A0)
getTestProgramFileIncludes p incls = getTestProgramIncludesByVer Fortran77Legacy incls p

getTestProgramIncludesByVer :: FortranVersion -> [FilePath] -> FilePath -> IO (ProgramFile A0)
getTestProgramIncludesByVer v incls p =
  flexReadFile p >>= versionedExpandedProgramFile v incls p

getTestProgramAnalysis :: String -> IO (ProgramFile (Analysis A0))
getTestProgramAnalysis = getTestProgramAnalysisByVer Fortran77Legacy []

getTestProgramAnalysisIncludes
  :: String -> [String] -> IO (ProgramFile (Analysis A0))
getTestProgramAnalysisIncludes p incls =
  getTestProgramAnalysisByVer Fortran77Legacy incls p

getTestProgramAnalysisByVer
  :: FortranVersion -> [FilePath] -> FilePath -> IO (ProgramFile (Analysis A0))
getTestProgramAnalysisByVer v incls p = do
  flexReadFile p >>= versionedExpandedProgramAnalysis v incls p

-- | Utility function to compare file content
compareFile :: FilePath -> FilePath -> IO Bool
compareFile expected actual = do
  c1 <- readFile expected
  c2 <- readFile actual
  diffFileContents c1 c2

diffFileContents :: String -> String -> IO Bool
diffFileContents s1 s2 = if s1 == s2
  then pure True
  else False <$ (putStrLn . ppDiff $ getDiff (toLines s1) (toLines s2))
  where toLines = fmap pure . lines
