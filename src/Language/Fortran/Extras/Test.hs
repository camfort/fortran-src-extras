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
getTestProgramFile p = do
  cts <- flexReadFile p
  versionedExpandedProgramFile Fortran77Legacy [] p cts

getTestProgramFileIncludes :: String -> [String] -> IO (ProgramFile A0)
getTestProgramFileIncludes p incls = do
  cts <- flexReadFile p
  versionedExpandedProgramFile Fortran77Legacy incls p cts

getTestProgramAnalysis :: String -> IO (ProgramFile (Analysis A0))
getTestProgramAnalysis p = do
  cts <- flexReadFile p
  versionedExpandedProgramAnalysis Fortran77Legacy [] p cts

getTestProgramAnalysisIncludes
  :: String -> [String] -> IO (ProgramFile (Analysis A0))
getTestProgramAnalysisIncludes p incls = do
  cts <- flexReadFile p
  versionedExpandedProgramAnalysis Fortran77Legacy incls p cts

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
