{-# LANGUAGE BlockArguments #-}

{-| Acceptance-test driver for the literate Haskell reference.

    Helpers here are shared across slices: walk a directory, pair @*A@/@*B@
    fixtures, parse a complete expression, compare via 'Binary.encode'.
    Later slices register additional groups without rewriting discovery.
-}
module Main where

import Codec.CBOR.Term (Term)
import Control.Monad (when)
import Crypto.Hash (Digest, SHA256)
import Data.List.NonEmpty (NonEmpty(..))
import System.FilePath ((</>))
import Test.Tasty (TestTree)
import Test.Tasty.Runners (NumThreads(..))

import qualified AlphaNormalization
import qualified BetaNormalization
import qualified Binary
import qualified Codec.CBOR.Term           as CBOR.Term
import qualified Codec.CBOR.Write          as CBOR.Write
import qualified Codec.Serialise           as Serialise
import qualified Crypto.Hash               as Hash
import qualified Data.ByteString           as ByteString
import qualified Data.List.NonEmpty        as NonEmpty
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

-- | Unique test name from a path under 'testsRoot'.
testName :: FilePath -> String
testName path = FilePath.makeRelative testsRoot path

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

    let name = testName inputFile

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
    let name = testName path

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

    let name = testName inputFile

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

maybeResolve
    :: Imports.ResolveEnv
    -> Syntax.Expression
    -> IO Syntax.Expression
maybeResolve env expression
    | not (containsImport expression) =
        return expression
    | otherwise = do
        resolved <- Imports.resolveExpression env expression
        case resolved of
            Left err ->
                fail (show err)
            Right value ->
                return value

-- | β-normalization: parse A and B; resolve imports if present; normalize only A.
betaNormalizationCase :: FilePath -> TestTree
betaNormalizationCase prefix = do
    let inputFile  = prefix <> "A.dhall"
    let outputFile = prefix <> "B.dhall"

    let name = testName inputFile

    HUnit.testCase name do
        input  <- expectParsed inputFile
        output <- expectParsed outputFile

        if containsImport input || containsImport output
            then withImportEnvironment inputFile do
                env <- resolveEnvFor inputFile Prelude.False
                resolvedInput  <- maybeResolve env input
                resolvedOutput <- maybeResolve env output
                assertEncodedEqual
                    "β-normalization mismatch"
                    resolvedOutput
                    (BetaNormalization.betaNormalize resolvedInput)
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

    let name = testName inputFile

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

    let name = testName inputFile

    HUnit.testCase name do
        input <- expectParsed inputFile
        expected <- Text.IO.readFile outputFile

        hashed <-
            if containsImport input
                then withImportEnvironment inputFile do
                    env <- resolveEnvFor inputFile Prelude.False
                    resolved <- maybeResolve env input
                    return (semanticHash resolved)
                else return (semanticHash input)

        HUnit.assertEqual
            "Semantic hash mismatch"
            (Text.strip expected)
            hashed

inferOrFail :: Syntax.Expression -> IO Syntax.Expression
inferOrFail expression =
    case TypeInference.inferType [] expression of
        Nothing       -> fail "Type inference failed"
        Just inferred -> return inferred

typeInferenceSuccessCase :: FilePath -> TestTree
typeInferenceSuccessCase prefix = do
    let inputFile  = prefix <> "A.dhall"
    let outputFile = prefix <> "B.dhall"

    let name = testName inputFile

    HUnit.testCase name do
        input  <- expectParsed inputFile
        output <- expectParsed outputFile

        inferred <-
            if containsImport input
                then withImportEnvironment inputFile do
                    env <- resolveEnvFor inputFile Prelude.False
                    resolved <- maybeResolve env input
                    inferOrFail resolved
                else inferOrFail input

        assertEncodedEqual "Type inference mismatch" output inferred

typeInferenceFailureCase :: FilePath -> TestTree
typeInferenceFailureCase path = do
    let name = testName path

    HUnit.testCase name do
        parsed <- parseFile path

        case parsed of
            Left _ ->
                return ()
            Right expression -> do
                inferred <-
                    if containsImport expression
                        then withImportEnvironment path do
                            env <- resolveEnvFor path Prelude.False
                            resolved <- Imports.resolveExpression env expression
                            case resolved of
                                Left _ ->
                                    return Nothing
                                Right value ->
                                    return (TypeInference.inferType [] value)
                        else return (TypeInference.inferType [] expression)
                case inferred of
                    Nothing -> return ()
                    Just _  -> HUnit.assertFailure "Unexpected successful type inference"

binaryDecodeFailureCase :: FilePath -> TestTree
binaryDecodeFailureCase path = do
    let name = testName path

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

copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory src dst = do
    Directory.createDirectoryIfMissing True dst
    names <- Directory.listDirectory src
    mapM_ copyOne names
  where
    copyOne name = do
        let from = src </> name
            to   = dst </> name
        isDir <- Directory.doesDirectoryExist from
        if isDir
            then copyDirectory from to
            else Directory.copyFile from to

asTextLiteral :: Syntax.Expression -> Maybe Text.Text
asTextLiteral (Syntax.TextLiteral (Syntax.Chunks [] text)) = Just text
asTextLiteral _ = Nothing

fromMapEntry :: Syntax.Expression -> Maybe (Text.Text, Syntax.Expression)
fromMapEntry (Syntax.RecordLiteral fields) = do
    keyExpr <- lookup "mapKey" fields
    valExpr <- lookup "mapValue" fields
    key <- asTextLiteral keyExpr
    return (key, valExpr)
fromMapEntry _ =
    Nothing

fromDhallMap :: Syntax.Expression -> Maybe [(Text.Text, Syntax.Expression)]
fromDhallMap (Syntax.EmptyList _) = Just []
fromDhallMap (Syntax.NonEmptyList xs) =
    traverse fromMapEntry (NonEmpty.toList xs)
fromDhallMap _ =
    Nothing

extractEnvVars :: Syntax.Expression -> Maybe [(String, String)]
extractEnvVars expression = do
    entries <- fromDhallMap (BetaNormalization.betaNormalize expression)
    pairs <- traverse (\(k, v) -> (,) (Text.unpack k) . Text.unpack <$> asTextLiteral v) entries
    return pairs

envFileFor :: FilePath -> FilePath
envFileFor inputFile =
    case stripSuffix "A.dhall" inputFile of
        Just prefix -> prefix <> "ENV.dhall"
        Nothing ->
            case stripSuffix ".dhall" inputFile of
                Just prefix -> prefix <> "ENV.dhall"
                Nothing     -> inputFile <> "ENV.dhall"

applyEnvFile :: FilePath -> IO ()
applyEnvFile path = do
    exists <- Directory.doesFileExist path
    when exists do
        expression <- expectParsed path
        case extractEnvVars expression of
            Nothing ->
                fail ("Could not read environment map from " <> path)
            Just bindings ->
                mapM_ (uncurry Environment.setEnv) bindings

resolveEnvFor :: FilePath -> Prelude.Bool -> IO Imports.ResolveEnv
resolveEnvFor inputFile useDisk = do
    testsAbs <- Directory.canonicalizePath testsRoot
    repoRoot <- Directory.canonicalizePath (testsAbs </> "..")
    parentOfRepo <- Directory.canonicalizePath (repoRoot </> "..")
    home <- Directory.canonicalizePath (testsRoot </> "import" </> "home")
    ancestor <- ancestorImport inputFile
    manager <- Imports.newInsecureManager
    cache <- Directory.canonicalizePath =<< Environment.getEnv "XDG_CACHE_HOME"

    Imports.newResolveEnv parentOfRepo home (ancestor :| []) manager cache useDisk

withImportEnvironment :: FilePath -> IO a -> IO a
withImportEnvironment inputFile action = do
    home <- Directory.canonicalizePath (testsRoot </> "import" </> "home")
    committedCache <- Directory.canonicalizePath (testsRoot </> "import" </> "cache")
    tmp <- Directory.getTemporaryDirectory
    let slug = map (\c -> if c == '/' || c == '\\' then '-' else c) inputFile
    let cache = tmp </> ("dhall-import-cache-" <> slug)
    Directory.removePathForcibly cache
    Directory.createDirectoryIfMissing True cache
    copyDirectory committedCache cache

    Environment.setEnv "HOME" home
    Environment.setEnv "XDG_CACHE_HOME" cache
    Environment.setEnv "DHALL_TEST_VAR" "6 * 7"
    Environment.unsetEnv "DHALL_HEADERS"
    Environment.unsetEnv "USER_AGENT"
    Environment.unsetEnv "XDG_CONFIG_HOME"

    applyEnvFile (envFileFor inputFile)

    action

importSuccessCase :: FilePath -> TestTree
importSuccessCase prefix = do
    let inputFile  = prefix <> "A.dhall"
    let outputFile = prefix <> "B.dhall"
    let name = testName inputFile

    HUnit.testCase name (withImportEnvironment inputFile do
        input  <- expectParsed inputFile
        output <- expectParsed outputFile
        env    <- resolveEnvFor inputFile True

        resolved <- Imports.resolveExpression env input

        case resolved of
            Left err ->
                fail (show err)
            Right expression ->
                assertEncodedEqual "Import resolution mismatch" output expression)

importFailureCase :: FilePath -> TestTree
importFailureCase path = do
    let name = testName path

    HUnit.testCase name (withImportEnvironment path do
        parsed <- parseFile path

        case parsed of
            Left _ ->
                return ()
            Right expression -> do
                env <- resolveEnvFor path True
                resolved <- Imports.resolveExpression env expression
                case resolved of
                    Left _  -> return ()
                    Right _ -> HUnit.assertFailure "Unexpected successful import resolution")

isEnvFile :: FilePath -> Bool
isEnvFile path =
    Text.isSuffixOf "ENV.dhall" (Text.pack path)

isImportSuccessA :: FilePath -> Bool
isImportSuccessA path =
    case stripSuffix "A.dhall" path of
        Just _  -> not (isEnvFile path)
        Nothing -> False

isImportFailureFile :: FilePath -> Bool
isImportFailureFile path =
    isDhallFile path && not (isEnvFile path)

-- | Naive import of Prelude is too slow for this reference: the full
-- package (@preludeA.dhall@) and per-file @prelude/**@ cases β-normalize
-- @assert@ examples (e.g. @List/shifted@ exceeds a 10-minute timeout).
-- Import-suite files such as @cors/PreludeA.dhall@ are unaffected.
isSlowPreludeA :: FilePath -> Bool
isSlowPreludeA path =
    let parts = FilePath.splitDirectories path
        name  = FilePath.takeFileName path
        inSlowSuite =
            "type-inference" `elem` parts
            || "semantic-hash" `elem` parts
    in  inSlowSuite
            && (name == "preludeA.dhall" || "prelude" `elem` parts)

isTypeInferenceSuccessA :: FilePath -> Bool
isTypeInferenceSuccessA path =
    case stripSuffix "A.dhall" path of
        Just _  -> not (isSlowPreludeA path)
        Nothing -> False

isSemanticHashA :: FilePath -> Bool
isSemanticHashA path =
    case stripSuffix "A.dhall" path of
        Just _  -> not (isSlowPreludeA path)
        Nothing -> False

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

    betaNormalization <-
        discoverBySuffix "A.dhall" betaNormalizationCase
            (testsRoot </> "normalization/success")

    let withTimeout =
            Tasty.localOption (Tasty.mkTimeout 30000000)  -- 30 seconds

    let withNormTimeout =
            Tasty.localOption (Tasty.mkTimeout 120000000)  -- 2 minutes

    let withLongTimeout =
            Tasty.localOption (Tasty.mkTimeout 600000000)  -- 10 minutes

    binaryDecodeSuccess <-
        discoverBySuffix "A.dhallb" binaryDecodeSuccessCase
            (testsRoot </> "binary-decode/success")

    binaryDecodeFailure <-
        discoverFiles isDhallbFile binaryDecodeFailureCase
            (testsRoot </> "binary-decode/failure")

    semanticHashTests <-
        discoverFiles
            isSemanticHashA
            (\path ->
                semanticHashCase
                    (maybe path id (stripSuffix "A.dhall" path)))
            (testsRoot </> "semantic-hash/success")

    typeInferenceSuccess <-
        discoverFiles
            isTypeInferenceSuccessA
            (\path ->
                typeInferenceSuccessCase
                    (maybe path id (stripSuffix "A.dhall" path)))
            (testsRoot </> "type-inference/success")

    typeInferenceFailureUnit <-
        discoverFiles isDhallFile typeInferenceFailureCase
            (testsRoot </> "type-inference/failure/unit")

    typeInferenceFailureTop <-
        discoverFilesHere isDhallFile typeInferenceFailureCase
            (testsRoot </> "type-inference/failure")

    importSuccess <-
        discoverFiles
            isImportSuccessA
            (\path -> importSuccessCase (maybe path id (stripSuffix "A.dhall" path)))
            (testsRoot </> "import/success")

    importFailure <-
        discoverFiles isImportFailureFile importFailureCase
            (testsRoot </> "import/failure")

    TestServer.withServers testsRoot
        (Tasty.defaultMain
            (Tasty.localOption (NumThreads 1)
                (Tasty.testGroup "Dhall acceptance tests"
                    [ Tasty.testGroup "parser"
                        [ parserSuccess
                        , parserFailure
                        ]
                    , Tasty.testGroup "alpha-normalization"
                        [ alphaNormalization ]
                    , withNormTimeout (Tasty.testGroup "normalization"
                        [ betaNormalization ]
                        )
                    , Tasty.testGroup "binary-decode"
                        [ binaryDecodeSuccess
                        , binaryDecodeFailure
                        ]
                    , withNormTimeout (Tasty.testGroup "semantic-hash"
                        [ semanticHashTests ]
                        )
                    , withTimeout (Tasty.testGroup "type-inference"
                        [ withLongTimeout typeInferenceSuccess
                        , Tasty.testGroup "failure"
                            [ typeInferenceFailureUnit
                            , typeInferenceFailureTop
                            ]
                        ]
                        )
                    , Tasty.testGroup "import"
                        [ importSuccess
                        , importFailure
                        ]
                    ]
                )
            )
        )

