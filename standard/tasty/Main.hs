{-# LANGUAGE BlockArguments #-}

module Main where

import System.FilePath ((</>))
import Test.Tasty (TestTree)

import qualified Binary
import qualified Codec.Serialise    as Serialise
import qualified Data.Text          as Text
import qualified Parser
import qualified System.Directory   as Directory
import qualified System.Environment as Environment
import qualified System.FilePath    as FilePath
import qualified Text.Earley        as Earley
import qualified Test.Tasty.HUnit   as HUnit
import qualified Test.Tasty         as Tasty

fileToTestTree :: FilePath -> TestTree
fileToTestTree prefix = do
    let inputFile  = prefix <> "A.dhall"
    let outputFile = prefix <> "B.dhallb"

    let name = FilePath.takeBaseName inputFile

    HUnit.testCase name do

        input <- readFile inputFile

        let (expressions, report) =
                Earley.fullParses (Earley.parser Parser.grammar) input

        expression <- case expressions of
           expression : _ -> return expression
           _              -> fail (show report)

        expectedTerm <- Serialise.readFileDeserialise outputFile

        let actualTerm = Binary.encode expression

        HUnit.assertEqual "Parsing test failure" expectedTerm actualTerm

inputFileToPrefix :: FilePath -> Maybe FilePath
inputFileToPrefix inputFile =
    fmap Text.unpack (Text.stripSuffix "A.dhall" (Text.pack inputFile))

directoryToTestTree :: FilePath -> IO TestTree
directoryToTestTree directory = do
    let name = FilePath.takeBaseName directory

    children <- Directory.listDirectory directory

    let process child = do
            let childPath = directory </> child

            isDirectory <- Directory.doesDirectoryExist childPath

            if isDirectory
                then do
                    testTree <- directoryToTestTree childPath

                    return [ testTree ]

                else do
                    case inputFileToPrefix childPath of
                        Just prefix -> do
                            return [ fileToTestTree prefix ]

                        Nothing -> do
                            return [ ]

    testTreess <- traverse process children

    return (Tasty.testGroup name (concat testTreess))

main :: IO ()
main = do
    Environment.setEnv "TASTY_HIDE_SUCCESSES" "true"

    testTree <- directoryToTestTree "../tests/parser/success"

    Tasty.defaultMain testTree
