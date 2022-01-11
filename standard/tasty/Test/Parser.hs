{-# LANGUAGE BlockArguments #-}

module Test.Parser where

import qualified Binary
import           Codec.CBOR.Term  (Term (..))
import qualified Codec.Serialise  as Serialise
import qualified Data.Text.IO     as Text.IO
import           Test.Tasty       (TestTree)
import qualified Test.Tasty       as Tasty
import qualified Test.Tasty.HUnit as HUnit
import           Test.Util        (TestFileType (..),
                                   directoryToFailureTestTree,
                                   directoryToSuccessTestTree, parseDhall,
                                   parseDhallOrFail)

-- | We need this because `NaN /= NaN`.  Grr…
assertEqualIncludingNaN :: String -> Term -> Term -> IO ()
assertEqualIncludingNaN _ (THalf l) (THalf r)
    | isNaN l && isNaN r =
        return ()
assertEqualIncludingNaN message expected actual =
    HUnit.assertEqual message expected actual

successTests :: IO TestTree
successTests = directoryToSuccessTestTree "../tests/parser/success" Dhall Dhallb \inputFile outputFile -> do
  input <- Text.IO.readFile inputFile

  expression <- parseDhallOrFail inputFile input

  expectedTerm <- Serialise.readFileDeserialise outputFile

  let actualTerm = Binary.encode expression

  assertEqualIncludingNaN "Parsing test failure" expectedTerm actualTerm

failureTests :: IO TestTree
failureTests = directoryToFailureTestTree "../tests/parser/failure" Dhall \inputFile -> do
  input <- Text.IO.readFile inputFile

  case parseDhall inputFile input of
      Left  _ -> return ()
      Right _ -> HUnit.assertFailure "Unexpected successful parse"

parserTests :: IO TestTree
parserTests = Tasty.testGroup "parser" <$> sequence [ successTests, failureTests ]
