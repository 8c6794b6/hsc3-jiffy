-- | Main module for running tests.
module Main where

-- hspec
import Test.Hspec

-- Internal
import BuilderTests
import ConvertTests
import Hsc3Tests

main :: IO ()
main =
  hspec (do builderTests
            convertTests
            hsc3Tests)
