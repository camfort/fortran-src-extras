module AnalysisSpec
  ( spec
  )
where

import           Test.Hspec
import           Data.Maybe                     ( isJust )
import           Language.Fortran.AST           ( A0
                                                , ProgramFile
                                                , Expression(..)
                                                , Statement(..)
                                                , Value(..)
                                                )
import           Language.Fortran.Analysis      ( Analysis )
import           Language.Fortran.Version       ( FortranVersion(..) )

import           Language.Fortran.Extras.Analysis
                                                ( versionedProgramAnalysis
                                                , versionedExpandedProgramAnalysis
                                                )
import           Language.Fortran.Util.Files
                                                ( flexReadFile )
import           Data.Generics.Uniplate.Data    ( universeBi )

wasIncluded :: ProgramFile (Analysis A0) -> String -> Bool
wasIncluded pa filename = any
  isJust
  [ included
  | StInclude _ _ (ExpValue _ _ (ValString incFilename)) included <- (universeBi pa :: [Statement (Analysis A0)])
  , incFilename == filename
  ]

spec :: Spec
spec = describe "ProgramAnalysis" $ do
  it "versionedProgramAnalysis - no includes expansion" $ do
    let path = "test/input_data/analysis/relative_include/include.f"
    pa <- versionedProgramAnalysis Fortran77Legacy path <$> flexReadFile path
    wasIncluded pa "includee.f" `shouldBe` False

  it "versionedExpandedProgramAnalysis - with expanded includes" $ do
    let path  = "test/input_data/analysis/with_include_path/include.f"
        idirs = ["test/input_data/analysis/with_include_path/includes"]
    contents <- flexReadFile path
    pa <- versionedExpandedProgramAnalysis Fortran77Legacy idirs path contents
    wasIncluded pa "includee.f" `shouldBe` True

  it "versionedExpandedProgramAnalysis - with expanded relative includes" $ do
    let path = "test/input_data/analysis/relative_include/include.f"
    contents <- flexReadFile path
    pa <- versionedExpandedProgramAnalysis Fortran77Legacy [] path contents
    wasIncluded pa "includee.f" `shouldBe` True
