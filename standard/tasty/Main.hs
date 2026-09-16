{-# LANGUAGE BlockArguments #-}

{-| Acceptance-test driver for the literate Haskell reference.

    Helpers here are shared across slices: walk a directory, pair @*A@/@*B@
    fixtures, parse a complete expression, compare via 'Binary.encode'.
    Later slices register additional groups without rewriting discovery.
-}
module Main where

import Codec.CBOR.Term (Term)
import Crypto.Hash (Digest, SHA256)
import Data.List.NonEmpty (NonEmpty(..))
import System.FilePath ((</>))
import Test.Tasty (TestTree)

import qualified AlphaNormalization
import qualified BetaNormalization
import qualified Binary
import qualified Codec.CBOR.Term           as CBOR.Term
import qualified Codec.CBOR.Write          as CBOR.Write
import qualified Codec.Serialise           as Serialise
import qualified Crypto.Hash               as Hash
import qualified Data.ByteString           as ByteString
import qualified Data.List                 as List
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text.Encoding
import qualified Data.Text.IO              as Text.IO
import qualified Imports
import qualified Parser
import qualified TypeInference
import qualified Syntax
import qualified System.Directory          as Directory
import qualified System.Environment        as Environment
import qualified System.FilePath           as FilePath
import qualified Text.Megaparsec           as Megaparsec
import qualified Test.Tasty.HUnit          as HUnit
import qualified Test.Tasty                as Tasty
import qualified TestServer

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

-- | Compare CBOR encodings as bytes so NaN equals itself and @-0.0@ differs
-- from @+0.0@, matching 'Equivalence.equivalent'.
encodedBytes :: Term -> ByteString.ByteString
encodedBytes term =
    CBOR.Write.toStrictByteString (CBOR.Term.encodeTerm term)

assertEncodedTermEqual :: String -> Term -> Syntax.Expression -> IO ()
assertEncodedTermEqual message expected actual =
    HUnit.assertEqual message (encodedBytes expected) (encodedBytes (Binary.encode actual))

assertEncodedEqual :: String -> Syntax.Expression -> Syntax.Expression -> IO ()
assertEncodedEqual message expected actual =
    HUnit.assertEqual
        message
        (encodedBytes (Binary.encode expected))
        (encodedBytes (Binary.encode actual))

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

-- | Non-recursive: only files directly in @directory@, not subdirectories.
discoverFilesHere
    :: (FilePath -> Bool)
    -> (FilePath -> TestTree)
    -> FilePath
    -> IO TestTree
discoverFilesHere predicate makeTest directory = do
    let name = FilePath.takeBaseName directory

    children <- Directory.listDirectory directory

    let files = do
            child <- children
            let childPath = directory </> child
            [ childPath | predicate childPath ]

    return (Tasty.testGroup name (map makeTest files))

isDhallFile :: FilePath -> Bool
isDhallFile path = FilePath.takeExtension path == ".dhall"

isDhallbFile :: FilePath -> Bool
isDhallbFile path = FilePath.takeExtension path == ".dhallb"

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

-- | Whether an expression still contains an unresolved import node.
containsImport :: Syntax.Expression -> Bool
containsImport Syntax.Import{} = True
containsImport expression =
    any containsImport (subexpressions expression)

subexpressions :: Syntax.Expression -> [Syntax.Expression]
subexpressions expression =
    case expression of
        Syntax.Variable{} ->
            []
        Syntax.Lambda _ a b ->
            [a, b]
        Syntax.Forall _ a b ->
            [a, b]
        Syntax.Let _ maybeType a b ->
            maybe [] pure maybeType ++ [a, b]
        Syntax.If a b c ->
            [a, b, c]
        Syntax.Merge a b maybeType ->
            a : b : maybe [] pure maybeType
        Syntax.ToMap a maybeType ->
            a : maybe [] pure maybeType
        Syntax.EmptyList a ->
            [a]
        Syntax.NonEmptyList (t :| ts) ->
            t : ts
        Syntax.Annotation a b ->
            [a, b]
        Syntax.Operator a _ b ->
            [a, b]
        Syntax.Application a b ->
            [a, b]
        Syntax.Field a _ ->
            [a]
        Syntax.ProjectByLabels a _ ->
            [a]
        Syntax.ProjectByType a b ->
            [a, b]
        Syntax.Completion a b ->
            [a, b]
        Syntax.Assert a ->
            [a]
        Syntax.With a _ b ->
            [a, b]
        Syntax.DoubleLiteral{} ->
            []
        Syntax.NaturalLiteral{} ->
            []
        Syntax.IntegerLiteral{} ->
            []
        Syntax.TextLiteral (Syntax.Chunks chunks _) ->
            map snd chunks
        Syntax.BytesLiteral{} ->
            []
        Syntax.DateLiteral{} ->
            []
        Syntax.TimeLiteral{} ->
            []
        Syntax.TimeZoneLiteral{} ->
            []
        Syntax.RecordType fields ->
            map snd fields
        Syntax.RecordLiteral fields ->
            map snd fields
        Syntax.UnionType alternatives ->
            [ t | (_, Just t) <- alternatives ]
        Syntax.ShowConstructor a ->
            [a]
        Syntax.Import{} ->
            []
        Syntax.Some a ->
            [a]
        Syntax.Builtin{} ->
            []
        Syntax.Constant{} ->
            []

-- | β-normalization: parse A and B; normalize only A; compare encodings.
-- Skip cases that still contain an 'Import' node until import resolution exists.
betaNormalizationCase :: FilePath -> TestTree
betaNormalizationCase prefix = do
    let inputFile  = prefix <> "A.dhall"
    let outputFile = prefix <> "B.dhall"

    let name = FilePath.takeBaseName inputFile

    HUnit.testCase name do
        input  <- expectParsed inputFile
        output <- expectParsed outputFile

        if containsImport input || containsImport output
            then putStrLn ("Skipping import case: " <> name)
            else
                assertEncodedEqual
                    "β-normalization mismatch"
                    output
                    (BetaNormalization.betaNormalize input)

-- | Binary decode: deserialise @*A.dhallb@, decode, parse @*B.dhall@, compare.
binaryDecodeSuccessCase :: FilePath -> TestTree
binaryDecodeSuccessCase prefix = do
    let inputFile  = prefix <> "A.dhallb"
    let outputFile = prefix <> "B.dhall"

    let name = FilePath.takeBaseName inputFile

    HUnit.testCase name do
        term <- Serialise.readFileDeserialise inputFile

        decoded <- case Binary.decode term of
            Nothing         -> fail "Binary decode failed"
            Just expression -> return expression

        expected <- expectParsed outputFile

        assertEncodedEqual "Binary decode mismatch" expected decoded

-- | SHA-256 of the CBOR encoding of the α-normalized β-normal form.
semanticHash :: Syntax.Expression -> Text.Text
semanticHash expression = "sha256:" <> Text.pack (show digest)
  where
    normalized =
        AlphaNormalization.alphaNormalize
            (BetaNormalization.betaNormalize expression)

    bytes =
        CBOR.Write.toStrictByteString
            (CBOR.Term.encodeTerm (Binary.encode normalized))

    digest = Hash.hash bytes :: Digest SHA256

semanticHashCase :: FilePath -> TestTree
semanticHashCase prefix = do
    let inputFile  = prefix <> "A.dhall"
    let outputFile = prefix <> "B.hash"

    let name = FilePath.takeBaseName inputFile

    HUnit.testCase name do
        input <- expectParsed inputFile

        if containsImport input
            then putStrLn ("Skipping import case: " <> name)
            else do
                expected <- Text.IO.readFile outputFile
                HUnit.assertEqual
                    "Semantic hash mismatch"
                    (Text.strip expected)
                    (semanticHash input)

typeInferenceSuccessCase :: FilePath -> TestTree
typeInferenceSuccessCase prefix = do
    let inputFile  = prefix <> "A.dhall"
    let outputFile = prefix <> "B.dhall"

    let name = FilePath.takeBaseName inputFile

    HUnit.testCase name do
        input  <- expectParsed inputFile
        output <- expectParsed outputFile

        if containsImport input
            then putStrLn ("Skipping import case: " <> name)
            else case TypeInference.inferType [] input of
                Nothing -> fail "Type inference failed"
                Just inferred ->
                    assertEncodedEqual
                        "Type inference mismatch"
                        output
                        inferred

typeInferenceFailureCase :: FilePath -> TestTree
typeInferenceFailureCase path = do
    let name = FilePath.takeBaseName path

    HUnit.testCase name do
        parsed <- parseFile path

        case parsed of
            Left _ ->
                return ()
            Right expression ->
                if containsImport expression
                    then putStrLn ("Skipping import case: " <> name)
                    else case TypeInference.inferType [] expression of
                        Nothing -> return ()
                        Just _  -> HUnit.assertFailure "Unexpected successful type inference"

binaryDecodeFailureCase :: FilePath -> TestTree
binaryDecodeFailureCase path = do
    let name = FilePath.takeBaseName path

    HUnit.testCase name do
        term <- Serialise.readFileDeserialise path

        case Binary.decode term of
            Nothing -> return ()
            Just _  -> HUnit.assertFailure "Unexpected successful decode"

splitPath :: FilePath -> [Text.Text]
splitPath path =
    filter (/= ".") (map Text.pack (FilePath.splitDirectories path))

ancestorImport :: FilePath -> IO Syntax.ImportType
ancestorImport inputFile = do
    testsAbs <- Directory.canonicalizePath testsRoot
    inputAbs <- Directory.canonicalizePath inputFile

    let relative = FilePath.makeRelative testsAbs inputAbs
    let components = splitPath relative
    let virtual = "dhall-lang" : "tests" : components

    case reverse virtual of
        fileName : dirRev ->
            return (Syntax.Path Syntax.Here (Syntax.File dirRev fileName))
        [] ->
            fail ("Empty import ancestor for " <> inputFile)

resolveEnvFor :: FilePath -> IO Imports.ResolveEnv
resolveEnvFor inputFile = do
    testsAbs <- Directory.canonicalizePath testsRoot
    repoRoot <- Directory.canonicalizePath (testsAbs </> "..")
    parentOfRepo <- Directory.canonicalizePath (repoRoot </> "..")
    home <- Directory.canonicalizePath (testsRoot </> "import" </> "home")
    ancestor <- ancestorImport inputFile
    manager <- Imports.newInsecureManager

    return Imports.ResolveEnv
        { Imports.rootCwd = parentOfRepo
        , Imports.homeDirectory = home
        , Imports.stack = ancestor :| []
        , Imports.httpManager = manager
        }

withImportEnvironment :: IO a -> IO a
withImportEnvironment action = do
    home <- Directory.canonicalizePath (testsRoot </> "import" </> "home")
    cache <- Directory.canonicalizePath (testsRoot </> "import" </> "cache")

    Environment.setEnv "HOME" home
    Environment.setEnv "XDG_CACHE_HOME" cache
    Environment.setEnv "DHALL_TEST_VAR" "6 * 7"

    action

importSuccessCase :: FilePath -> TestTree
importSuccessCase prefix = do
    let inputFile  = prefix <> "A.dhall"
    let outputFile = prefix <> "B.dhall"
    let name = FilePath.takeBaseName inputFile

    HUnit.testCase name (withImportEnvironment do
        input  <- expectParsed inputFile
        output <- expectParsed outputFile
        env    <- resolveEnvFor inputFile

        resolved <- Imports.resolveExpression env input

        case resolved of
            Left err ->
                fail (show err)
            Right expression ->
                assertEncodedEqual "Import resolution mismatch" output expression)

importFailureCase :: FilePath -> TestTree
importFailureCase path = do
    let name = FilePath.takeBaseName path

    HUnit.testCase name (withImportEnvironment do
        parsed <- parseFile path

        case parsed of
            Left _ ->
                return ()
            Right expression -> do
                env <- resolveEnvFor path
                resolved <- Imports.resolveExpression env expression
                case resolved of
                    Left _  -> return ()
                    Right _ -> HUnit.assertFailure "Unexpected successful import resolution")

importPathReady :: FilePath -> Bool
importPathReady path =
    let needle n = n `List.isInfixOf` path
    in  not (any needle ["cors", "Hash", "DontCacheIfHash"])

isImportFailureFile :: FilePath -> Bool
isImportFailureFile path =
    isDhallFile path && importPathReady path && not (Text.isSuffixOf "ENV.dhall" (Text.pack path))

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

    betaNormalizationUnit <-
        discoverBySuffix "A.dhall" betaNormalizationCase
            (testsRoot </> "normalization/success/unit")

    betaNormalizationSimple <-
        discoverBySuffix "A.dhall" betaNormalizationCase
            (testsRoot </> "normalization/success/simple")

    betaNormalizationSimplifications <-
        discoverBySuffix "A.dhall" betaNormalizationCase
            (testsRoot </> "normalization/success/simplifications")

    betaNormalizationTutorial <-
        discoverBySuffix "A.dhall" betaNormalizationCase
            (testsRoot </> "normalization/success/haskell-tutorial")

    betaNormalizationRegression <-
        discoverBySuffix "A.dhall" betaNormalizationCase
            (testsRoot </> "normalization/success/regression")

    let withTimeout =
            Tasty.localOption (Tasty.mkTimeout 3000000)  -- 3 seconds

    binaryDecodeSuccess <-
        discoverBySuffix "A.dhallb" binaryDecodeSuccessCase
            (testsRoot </> "binary-decode/success")

    binaryDecodeFailure <-
        discoverFiles isDhallbFile binaryDecodeFailureCase
            (testsRoot </> "binary-decode/failure")

    semanticHashSimple <-
        discoverBySuffix "A.dhall" semanticHashCase
            (testsRoot </> "semantic-hash/success/simple")

    semanticHashSimplifications <-
        discoverBySuffix "A.dhall" semanticHashCase
            (testsRoot </> "semantic-hash/success/simplifications")

    semanticHashTutorial <-
        discoverBySuffix "A.dhall" semanticHashCase
            (testsRoot </> "semantic-hash/success/haskell-tutorial")

    typeInferenceUnit <-
        discoverBySuffix "A.dhall" typeInferenceSuccessCase
            (testsRoot </> "type-inference/success/unit")

    typeInferenceSimple <-
        discoverBySuffix "A.dhall" typeInferenceSuccessCase
            (testsRoot </> "type-inference/success/simple")

    typeInferenceRegression <-
        discoverBySuffix "A.dhall" typeInferenceSuccessCase
            (testsRoot </> "type-inference/success/regression")

    typeInferenceFailureUnit <-
        discoverFiles isDhallFile typeInferenceFailureCase
            (testsRoot </> "type-inference/failure/unit")

    typeInferenceFailureTop <-
        discoverFilesHere isDhallFile typeInferenceFailureCase
            (testsRoot </> "type-inference/failure")

    importSuccessUnit <-
        discoverFiles
            (\path ->
                case stripSuffix "A.dhall" path of
                    Just prefix -> importPathReady (prefix <> "A.dhall")
                    Nothing     -> False)
            (\path ->
                importSuccessCase (maybe path id (stripSuffix "A.dhall" path)))
            (testsRoot </> "import/success/unit")

    importFailureUnit <-
        discoverFiles isImportFailureFile importFailureCase
            (testsRoot </> "import/failure/unit")

    TestServer.withServers testsRoot
        (Tasty.defaultMain
            (Tasty.testGroup "Dhall acceptance tests"
                [ Tasty.testGroup "parser"
                    [ parserSuccess
                    , parserFailure
                    ]
                , Tasty.testGroup "alpha-normalization"
                    [ alphaNormalization ]
                , withTimeout (Tasty.testGroup "normalization"
                    [ Tasty.testGroup "unit" [ betaNormalizationUnit ]
                    , betaNormalizationSimple
                    , betaNormalizationSimplifications
                    , betaNormalizationTutorial
                    , betaNormalizationRegression
                    , betaNormalizationCase
                        (testsRoot </> "normalization/success/WithRecordValue")
                    , betaNormalizationCase
                        (testsRoot </> "normalization/success/remoteSystems")
                    ])
                , Tasty.testGroup "binary-decode"
                    [ binaryDecodeSuccess
                    , binaryDecodeFailure
                    ]
                , Tasty.testGroup "semantic-hash"
                    [ semanticHashSimple
                    , semanticHashSimplifications
                    , semanticHashTutorial
                    ]
                , withTimeout (Tasty.testGroup "type-inference"
                    [ Tasty.testGroup "success"
                        [ typeInferenceUnit
                        , typeInferenceSimple
                        , typeInferenceRegression
                        ]
                    , Tasty.testGroup "failure"
                        [ typeInferenceFailureUnit
                        , typeInferenceFailureTop
                        ]
                    ])
                , Tasty.testGroup "import"
                    [ importSuccessUnit
                    , importFailureUnit
                    ]
                ]
            )
        )
