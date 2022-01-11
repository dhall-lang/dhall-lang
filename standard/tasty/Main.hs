module Main where

import qualified System.Environment      as Environment
import qualified Test.AlphaNormalization
import qualified Test.Parser
import qualified Test.Tasty              as Tasty

main :: IO ()
main = do
    Environment.setEnv "TASTY_HIDE_SUCCESSES" "true"

    parserTests <- Test.Parser.parserTests
    alphaNormalizationTests <- Test.AlphaNormalization.alphaNormalizationTests

    let tests = Tasty.testGroup "tests" [ parserTests, alphaNormalizationTests ]

    Tasty.defaultMain tests
