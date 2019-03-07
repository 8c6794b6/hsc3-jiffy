-- | Main module for running tests.
module Main where

-- hspec
import Test.Hspec

-- Internal
import ConvertTests
import Hsc3Tests

main :: IO ()
main =
  hspec (do convertTests
            hsc3Tests)
