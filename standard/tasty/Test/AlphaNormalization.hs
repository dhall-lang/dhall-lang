{-# LANGUAGE BlockArguments #-}

module Test.AlphaNormalization (alphaNormalizationTests) where
import           AlphaNormalization (alphaNormalize)
import qualified Data.Text.IO       as Text.IO
import           Test.Tasty         (TestTree)
import qualified Test.Tasty         as Tasty
import qualified Test.Tasty.HUnit   as HUnit
import           Test.Util          (TestFileType (Dhall),
                                     directoryToSuccessTestTree,
                                     parseDhallOrFail)

successTests :: IO TestTree
successTests = directoryToSuccessTestTree "../tests/alpha-normalization/success" Dhall Dhall \inputFile outputFile -> do
  output <- Text.IO.readFile outputFile
  input <- Text.IO.readFile inputFile

  outputExpression <- parseDhallOrFail outputFile output
  inputExpression <- parseDhallOrFail inputFile input

  let expectedExpression = alphaNormalize outputExpression
  let actualExpression   = alphaNormalize inputExpression

  HUnit.assertEqual "Alpha normalization test failure" expectedExpression actualExpression

alphaNormalizationTests :: IO TestTree
alphaNormalizationTests = Tasty.testGroup "alpha normalization" <$> sequence [ successTests ]
