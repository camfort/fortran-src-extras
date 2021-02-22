module ExtrasSpec where

import           Test.Hspec

import           Control.Exception
import           System.Exit                    ( ExitCode )
import           System.IO                      ( stderr
                                                , stdout
                                                )
import           System.IO.Silently             ( hCapture_ )

import           Language.Fortran.Extras
                                                ( errorHandler )

spec :: Spec
spec = describe "errorHandler" $ do
  let
    doErr s = do
      _ <-
        try $ errorHandler "foo" . Left . toException $ AssertionFailed s :: IO
          (Either ExitCode ())
      return ()
  it "Prints to stderr" $ do
    out <- hCapture_ [stderr] $ doErr "stderr"
    null out `shouldBe` False
  it "Doesn't print to stdout" $ do
    out <- hCapture_ [stdout] $ doErr "stdout"
    null out `shouldBe` True
