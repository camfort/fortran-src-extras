module ExtrasSpec where

import           Test.Hspec

import           Control.Exception
import           System.Exit                    ( ExitCode )
import           System.IO                      ( stderr
                                                , stdout
                                                )
import           System.IO.Silently             ( hCapture_ )

import           Language.Fortran.Util.Position ( Position(..)
                                                , SrcSpan(..)
                                                )
import           Language.Fortran.Extras
                                                ( errorHandler )
import           Language.Fortran.Extras.Encoding

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
  it "Shows file span" $ do
    showFileSpan (SrcSpan (Position 1 2 3 "filepath" Nothing) (Position 4 5 6 "filepath" Nothing)) `shouldBe` "filepath-(3:1)-(6:4)"
