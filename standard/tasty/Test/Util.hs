{-# LANGUAGE BlockArguments #-}
module Test.Util (directoryToSuccessTestTree, directoryToFailureTestTree, TestFileType(..), parseDhall, parseDhallOrFail) where

import           Data.Text        (Text)
import qualified Data.Text        as Text
import           Data.Void        (Void)
import qualified Parser
import           Syntax           (Expression)
import qualified System.Directory as Directory
import           System.FilePath  (takeBaseName, (</>))
import qualified System.FilePath  as FilePath
import           Test.Tasty       (TestTree)
import qualified Test.Tasty       as Tasty
import           Test.Tasty.HUnit (Assertion)
import qualified Test.Tasty.HUnit as HUnit
import qualified Text.Megaparsec  as Megaparsec

parseDhall :: FilePath -> Text -> Either (Megaparsec.ParseErrorBundle Text Void) Expression
parseDhall = Megaparsec.runParser (Parser.unParser (Parser.completeExpression <* Megaparsec.eof))

parseDhallOrFail :: MonadFail m => FilePath -> Text -> m Expression
parseDhallOrFail inputFile input =
  case parseDhall inputFile input of
    Left  errors     -> fail (Megaparsec.errorBundlePretty errors)
    Right expression -> return expression

inputFileToPrefix :: FilePath -> Text -> Maybe FilePath
inputFileToPrefix inputFile suffix =
    fmap Text.unpack (Text.stripSuffix suffix (Text.pack inputFile))

data TestFileType = Dhall | Dhallb

directoryToSuccessTestTree :: FilePath -> TestFileType -> TestFileType -> (FilePath -> FilePath -> Assertion) -> IO TestTree
directoryToSuccessTestTree directory inputType outputType fileToTestTree = do
    let name = FilePath.takeBaseName directory

    children <- Directory.listDirectory directory

    let process child = do
            let childPath = directory </> child

            isDirectory <- Directory.doesDirectoryExist childPath

            if isDirectory
                then do
                    testTree <- directoryToSuccessTestTree childPath inputType outputType fileToTestTree

                    return [ testTree ]

                else do
                    let inputSuffix = case inputType of
                          Dhall  -> "A.dhall"
                          Dhallb -> "A.dhallb"

                    let outputSuffix = case outputType of
                          Dhall  -> "B.dhall"
                          Dhallb -> "B.dhallb"

                    case inputFileToPrefix childPath inputSuffix of
                        Just prefix ->
                          return [ HUnit.testCase (takeBaseName prefix) (fileToTestTree childPath (prefix ++ outputSuffix)) ]

                        Nothing -> do
                            return [ ]

    testTreess <- traverse process children

    return (Tasty.testGroup name (concat testTreess))

directoryToFailureTestTree :: FilePath -> TestFileType -> (FilePath -> Assertion) -> IO TestTree
directoryToFailureTestTree directory inputType fileToTestTree = do
    let name = FilePath.takeBaseName directory

    children <- Directory.listDirectory directory

    let process child = do
            let childPath = directory </> child

            isDirectory <- Directory.doesDirectoryExist childPath

            if isDirectory
                then do
                    testTree <- directoryToFailureTestTree childPath inputType fileToTestTree

                    return [ testTree ]

                else do
                    return [ HUnit.testCase (takeBaseName childPath) (fileToTestTree childPath) ]


    testTreess <- traverse process children

    return (Tasty.testGroup name (concat testTreess))
