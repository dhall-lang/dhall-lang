{-# LANGUAGE BlockArguments #-}

{-| Acceptance-test driver for the literate Haskell reference.

    Helpers here are shared across slices: walk a directory, pair @*A@/@*B@
    fixtures, parse a complete expression, compare via 'Binary.encode'.
    Later slices register additional groups without rewriting discovery.
-}
module Main where

import Codec.CBOR.Term (Term(..))
import System.FilePath ((</>))
import Test.Tasty (TestTree)

import qualified AlphaNormalization
import qualified Binary
import qualified Codec.Serialise           as Serialise
import qualified Data.ByteString           as ByteString
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text.Encoding
import qualified Parser
import qualified Syntax
import qualified System.Directory          as Directory
import qualified System.Environment        as Environment
import qualified System.FilePath           as FilePath
import qualified Text.Megaparsec           as Megaparsec
import qualified Test.Tasty.HUnit          as HUnit
import qualified Test.Tasty                as Tasty

-- | Relative to @standard/@ when running @cabal test@.  Nix rewrites this
-- prefix to an absolute store path in @postPatch@.
testsRoot :: FilePath
testsRoot = "../tests"

-- | Parse a complete Dhall expression, requiring the whole file to be consumed.
parseExpression :: FilePath -> Text.Text -> Either String Syntax.Expression
parseExpression sourcePath input = do
    let parser = Parser.unParser do
            e <- Parser.completeExpression

            Megaparsec.eof

            return e

    case Megaparsec.runParser parser sourcePath input of
        Left  errors     -> Left (Megaparsec.errorBundlePretty errors)
        Right expression -> Right expression

-- | Read a @.dhall@ file as UTF-8.  Invalid encoding is a parse failure.
readDhallFile :: FilePath -> IO (Either String Text.Text)
readDhallFile path = do
    bytes <- ByteString.readFile path

    case Text.Encoding.decodeUtf8' bytes of
        Left  exception -> return (Left (show exception))
        Right text      -> return (Right text)

parseFile :: FilePath -> IO (Either String Syntax.Expression)
parseFile path = do
    decoded <- readDhallFile path

    return (decoded >>= parseExpression path)

expectParsed :: FilePath -> IO Syntax.Expression
expectParsed path = do
    parsed <- parseFile path

    case parsed of
        Left  errors     -> fail errors
        Right expression -> return expression

-- | We need this because @NaN /= NaN@.
assertEqualIncludingNaN :: String -> Term -> Term -> IO ()
assertEqualIncludingNaN _ (THalf l) (THalf r)
    | isNaN l && isNaN r =
        return ()
assertEqualIncludingNaN message expected actual =
    HUnit.assertEqual message expected actual

assertEncodedTermEqual :: String -> Term -> Syntax.Expression -> IO ()
assertEncodedTermEqual message expected actual =
    assertEqualIncludingNaN message expected (Binary.encode actual)

assertEncodedEqual :: String -> Syntax.Expression -> Syntax.Expression -> IO ()
assertEncodedEqual message expected actual =
    assertEqualIncludingNaN message (Binary.encode expected) (Binary.encode actual)

stripSuffix :: Text.Text -> FilePath -> Maybe FilePath
stripSuffix suffix path =
    fmap Text.unpack (Text.stripSuffix suffix (Text.pack path))

-- | Recursively collect files under a directory.
listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive directory = do
    children <- Directory.listDirectory directory

    let process child = do
            let childPath = directory </> child

            isDirectory <- Directory.doesDirectoryExist childPath

            if isDirectory
                then listFilesRecursive childPath
                else return [ childPath ]

    concat <$> traverse process children

-- | Build a 'TestTree' from every file whose name ends in the given suffix.
-- The callback receives the path with the suffix stripped (a \"prefix\").
discoverBySuffix
    :: Text.Text
    -> (FilePath -> TestTree)
    -> FilePath
    -> IO TestTree
discoverBySuffix suffix makeTest directory = do
    let name = FilePath.takeBaseName directory

    files <- listFilesRecursive directory

    let tests = do
            file <- files

            prefix <- maybe [] (\p -> [p]) (stripSuffix suffix file)

            return (makeTest prefix)

    return (Tasty.testGroup name tests)

-- | Same as 'discoverBySuffix', but the callback receives the full path.
discoverFiles
    :: (FilePath -> Bool)
    -> (FilePath -> TestTree)
    -> FilePath
    -> IO TestTree
discoverFiles predicate makeTest directory = do
    let name = FilePath.takeBaseName directory

    files <- listFilesRecursive directory

    let tests = map makeTest (filter predicate files)

    return (Tasty.testGroup name tests)

isDhallFile :: FilePath -> Bool
isDhallFile path = FilePath.takeExtension path == ".dhall"

-- | Parser success: parse @*A.dhall@, encode, compare to @*B.dhallb@.
parserSuccessCase :: FilePath -> TestTree
parserSuccessCase prefix = do
    let inputFile  = prefix <> "A.dhall"
    let outputFile = prefix <> "B.dhallb"

    let name = FilePath.takeBaseName inputFile

    HUnit.testCase name do
        parsed <- parseFile inputFile

        expression <- case parsed of
            Left  errors     -> fail errors
            Right expression -> return expression

        expectedTerm <- Serialise.readFileDeserialise outputFile

        assertEncodedTermEqual "Parsing test failure" expectedTerm expression

-- | Parser failure: the file must not parse as a complete expression.
parserFailureCase :: FilePath -> TestTree
parserFailureCase path = do
    let name = FilePath.takeBaseName path

    HUnit.testCase name do
        parsed <- parseFile path

        case parsed of
            Left  _ -> return ()
            Right _ -> HUnit.assertFailure "Unexpected successful parse"

-- | α-normalization: parse A and B, α-normalize both, compare encodings.
alphaNormalizationCase :: FilePath -> TestTree
alphaNormalizationCase prefix = do
    let inputFile  = prefix <> "A.dhall"
    let outputFile = prefix <> "B.dhall"

    let name = FilePath.takeBaseName inputFile

    HUnit.testCase name do
        input  <- expectParsed inputFile
        output <- expectParsed outputFile

        assertEncodedEqual
            "α-normalization mismatch"
            (AlphaNormalization.alphaNormalize output)
            (AlphaNormalization.alphaNormalize input)

main :: IO ()
main = do
    Environment.setEnv "TASTY_HIDE_SUCCESSES" "true"

    parserSuccess <-
        discoverBySuffix "A.dhall" parserSuccessCase
            (testsRoot </> "parser/success")

    parserFailure <-
        discoverFiles isDhallFile parserFailureCase
            (testsRoot </> "parser/failure")

    alphaNormalization <-
        discoverBySuffix "A.dhall" alphaNormalizationCase
            (testsRoot </> "alpha-normalization/success")

    Tasty.defaultMain
        (Tasty.testGroup "Dhall acceptance tests"
            [ Tasty.testGroup "parser"
                [ parserSuccess
                , parserFailure
                ]
            , Tasty.testGroup "alpha-normalization"
                [ alphaNormalization ]
            ]
        )
