# Import resolution (reference implementation)

Literate Haskell for the algorithm in `imports-implementation-notes.md` and
the chaining / canonicalization / `as Location` judgments in `imports.md`.

```haskell
module Imports
    ( ImportError(..)
    , ResolveEnv(..)
    , newInsecureManager
    , resolveExpression
    , canonicalizeImport
    , chainImports
    ) where

import Control.Exception (SomeException, try)
import Crypto.Hash (Digest, SHA256)
import Data.List.NonEmpty (NonEmpty(..))
import Network.HTTP.Client (Manager)
import Prelude hiding (Bool(..))
import System.FilePath ((</>))
import Syntax

import qualified BetaNormalization
import qualified Data.ByteString           as ByteString
import qualified Data.ByteString.Lazy      as ByteString.Lazy
import qualified Data.List.NonEmpty        as NonEmpty
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text.Encoding
import qualified Network.Connection        as Connection
import qualified Network.HTTP.Client       as HTTP
import qualified Network.HTTP.Client.TLS   as HTTP.TLS
import qualified Network.HTTP.Types        as HTTP.Types
import qualified Parser
import qualified Prelude
import qualified System.Directory          as Directory
import qualified System.Environment        as Environment
import qualified Text.Megaparsec           as Megaparsec
import qualified TypeInference

data ImportError
    = SoftFailure String
    | HardFailure String
    deriving (Show)

data ResolveEnv = ResolveEnv
    { rootCwd        :: FilePath
    , homeDirectory  :: FilePath
    , stack          :: NonEmpty ImportType
    , httpManager    :: Manager
    }

locationType :: Expression
locationType =
    UnionType
        [ ("Environment", Just (Builtin Text))
        , ("Local"      , Just (Builtin Text))
        , ("Missing"    , Nothing)
        , ("Remote"     , Just (Builtin Text))
        ]

inject :: Text -> Text -> Expression
inject alternative text =
    Application
        (Field locationType alternative)
        (TextLiteral (Chunks [] text))

injectMissing :: Expression
injectMissing = Field locationType "Missing"

canonicalizeDirectory :: [Text] -> [Text]
canonicalizeDirectory reversedComponents =
    reverse (foldl step [] (reverse reversedComponents))
  where
    step acc "." = acc
    step acc ".." =
        case acc of
            []                       -> [".."]
            xs | last xs == ".."     -> xs ++ [".."]
            xs                       -> init xs
    step acc component = acc ++ [component]

canonicalizeFile :: File -> File
canonicalizeFile (File dir name) =
    File (canonicalizeDirectory dir) name

canonicalizeImport :: ImportType -> ImportType
canonicalizeImport Missing = Missing
canonicalizeImport (Env x) = Env x
canonicalizeImport (Path prefix file₀) =
    Path prefix (canonicalizeFile file₀)
canonicalizeImport (Remote url headers) =
    Remote url{ path = canonicalizeFile (path url) } headers

chainDirectory :: [Text] -> [Text] -> [Text]
chainDirectory parentDir childDir =
    childDir ++ parentDir

chainFiles :: File -> File -> File
chainFiles parent child =
    File (chainDirectory (directory parent) (directory child)) (file child)

chainParentDotDot :: File -> File -> File
chainParentDotDot parent child =
    File (chainDirectory (directory parent) (directory child ++ [".."])) (file child)

chainImports :: ImportType -> ImportType -> ImportType
chainImports _ Missing =
    Missing
chainImports _ (Env x) =
    Env x
chainImports _ child@(Path Absolute _) =
    canonicalizeImport child
chainImports _ child@(Path Home _) =
    canonicalizeImport child
chainImports _ child@(Remote _ _) =
    canonicalizeImport child
chainImports (Path prefix parent) (Path Here child) =
    canonicalizeImport (Path prefix (chainFiles parent child))
chainImports (Path prefix parent) (Path Parent child) =
    canonicalizeImport (Path prefix (chainParentDotDot parent child))
chainImports (Remote url headers) (Path Here child) =
    canonicalizeImport
        (Remote url{ path = chainFiles (path url) child } headers)
chainImports (Remote url headers) (Path Parent child) =
    canonicalizeImport
        (Remote url{ path = chainParentDotDot (path url) child } headers)
chainImports _ child =
    canonicalizeImport child

renderComponents :: [Text] -> Text
renderComponents = Text.intercalate "/"

renderFile :: File -> Text
renderFile (File dir name) =
    renderComponents (reverse dir ++ [name])

renderPath :: FilePrefix -> File -> Text
renderPath Absolute file₀ = "/" <> renderFile file₀
renderPath Here     file₀ = "./" <> renderFile file₀
renderPath Parent   file₀ = "../" <> renderFile file₀
renderPath Home     file₀ = "~/" <> renderFile file₀

renderURL :: URL -> Text
renderURL (URL scheme₀ authority₀ file₀ query₀) =
    schemeText <> "://" <> authority₀ <> "/" <> renderFile file₀ <> queryText
  where
    schemeText =
        case scheme₀ of
            HTTP  -> "http"
            HTTPS -> "https"
    queryText =
        case query₀ of
            Nothing    -> ""
            Just q -> "?" <> q

prettyImportType :: ImportType -> Text
prettyImportType Missing = "missing"
prettyImportType (Env x) = "env:" <> x
prettyImportType (Path prefix file₀) = renderPath prefix file₀
prettyImportType (Remote url _) = renderURL url

asLocation :: ImportType -> Expression
asLocation Missing =
    injectMissing
asLocation (Env x) =
    inject "Environment" x
asLocation (Path prefix file₀) =
    inject "Local" (renderPath prefix file₀)
asLocation (Remote url _) =
    inject "Remote" (renderURL url)

parseExpression :: FilePath -> Text -> Either String Expression
parseExpression sourcePath input = do
    let parser = Parser.unParser do
            e <- Parser.completeExpression
            Megaparsec.eof
            return e

    case Megaparsec.runParser parser sourcePath input of
        Left  errors     -> Left (Megaparsec.errorBundlePretty errors)
        Right expression -> Right expression

localFilePath :: ResolveEnv -> FilePrefix -> File -> IO FilePath
localFilePath _env Absolute file₀ =
    return (Text.unpack (renderPath Absolute file₀))
localFilePath env Home file₀ = do
    let rest = Text.unpack (renderFile file₀)
    return (homeDirectory env </> rest)
localFilePath env Here file₀ = do
    let rest = Text.unpack (renderFile file₀)
    return (rootCwd env </> rest)
localFilePath env Parent file₀ = do
    let rest = Text.unpack (renderFile file₀)
    return (rootCwd env </> ".." </> rest)

onStack :: ImportType -> NonEmpty ImportType -> Prelude.Bool
onStack child (p :| ps) =
    prettyImportType child == prettyImportType p
        || any (\q -> prettyImportType child == prettyImportType q) ps

referentiallySane :: ImportType -> ImportType -> Prelude.Bool
referentiallySane (Remote _ _) (Path Absolute _) = Prelude.False
referentiallySane (Remote _ _) (Path Here _)     = Prelude.False
referentiallySane (Remote _ _) (Path Parent _)   = Prelude.False
referentiallySane (Remote _ _) (Path Home _)     = Prelude.False
referentiallySane (Remote _ _) (Env _)           = Prelude.False
referentiallySane _ _                            = Prelude.True

fetchHTTP :: Manager -> URL -> IO (Either ImportError ByteString.ByteString)
fetchHTTP manager url = do
    request <- HTTP.parseRequest (Text.unpack (renderURL url))
    result <- try (HTTP.httpLbs request manager) :: IO (Either SomeException (HTTP.Response ByteString.Lazy.ByteString))
    case result of
        Left exception ->
            return (Left (SoftFailure (show exception)))
        Right response ->
            let status = HTTP.Types.statusCode (HTTP.responseStatus response)
            in  if status >= 200 && status < 300
                    then return (Right (ByteString.Lazy.toStrict (HTTP.responseBody response)))
                    else
                        return
                            (Left (SoftFailure ("HTTP " <> show status)))

readBytes :: ResolveEnv -> ImportType -> IO (Either ImportError ByteString.ByteString)
readBytes _ Missing =
    return (Left (SoftFailure "missing"))
readBytes _ (Env name) = do
    mValue <- Environment.lookupEnv (Text.unpack name)
    case mValue of
        Nothing ->
            return (Left (SoftFailure ("unset environment variable: " <> Text.unpack name)))
        Just value ->
            return (Right (Text.Encoding.encodeUtf8 (Text.pack value)))
readBytes env (Path prefix file₀) = do
    filePath <- localFilePath env prefix file₀
    exists <- Directory.doesFileExist filePath
    if not exists
        then return (Left (SoftFailure ("missing file: " <> filePath)))
        else Right <$> ByteString.readFile filePath
readBytes env (Remote url _) =
    fetchHTTP (httpManager env) url

resolveImport
    :: ResolveEnv
    -> ImportType
    -> ImportMode
    -> Maybe (Digest SHA256)
    -> IO (Either ImportError Expression)
resolveImport env rawChild mode _hash = do
    let parent = NonEmpty.head (stack env)
    let child  = chainImports parent rawChild

    case mode of
        Location ->
            return (Right (asLocation child))
        _ ->
            resolveFetched env child mode

resolveFetched
    :: ResolveEnv
    -> ImportType
    -> ImportMode
    -> IO (Either ImportError Expression)
resolveFetched env child mode
    | onStack child (stack env) =
        return (Left (HardFailure ("cyclic import: " <> Text.unpack (prettyImportType child))))
    | not (referentiallySane (NonEmpty.head (stack env)) child)
        && mode /= Location =
        return (Left (HardFailure "referential sanity"))
    | otherwise = do
        bytes <- readBytes env child
        case bytes of
            Left err -> return (Left err)
            Right raw ->
                case mode of
                    RawBytes ->
                        return (Right (BytesLiteral raw))
                    RawText ->
                        case Text.Encoding.decodeUtf8' raw of
                            Left  exception ->
                                return (Left (HardFailure (show exception)))
                            Right text ->
                                return (Right (TextLiteral (Chunks [] text)))
                    Location ->
                        return (Right (asLocation child))
                    Code ->
                        resolveCode env child raw

resolveCode
    :: ResolveEnv
    -> ImportType
    -> ByteString.ByteString
    -> IO (Either ImportError Expression)
resolveCode env child raw =
    case Text.Encoding.decodeUtf8' raw of
        Left exception ->
            return (Left (HardFailure (show exception)))
        Right text ->
            case parseExpression (Text.unpack (prettyImportType child)) text of
                Left errors ->
                    return (Left (HardFailure errors))
                Right parsed -> do
                    let env' = env{ stack = child :| NonEmpty.toList (stack env) }
                    resolved <- resolveExpression env' parsed
                    case resolved of
                        Left err ->
                            return (Left err)
                        Right expression ->
                            case TypeInference.inferType [] expression of
                                Nothing ->
                                    return (Left (HardFailure "imported expression is ill-typed"))
                                Just _ ->
                                    return (Right (BetaNormalization.betaNormalize expression))

resolveExpression
    :: ResolveEnv
    -> Expression
    -> IO (Either ImportError Expression)
resolveExpression env expression =
    case expression of
        Import importType mode hash ->
            resolveImport env importType mode hash
        Operator left Alternative right -> do
            leftResult <- resolveExpression env left
            case leftResult of
                Right resolved ->
                    return (Right resolved)
                Left (SoftFailure _) ->
                    resolveExpression env right
                Left err ->
                    return (Left err)
        other ->
            walk other
  where
    go = resolveExpression env

    walk e =
        case e of
            Variable{} -> return (Right e)
            Builtin{} -> return (Right e)
            Constant{} -> return (Right e)
            DoubleLiteral{} -> return (Right e)
            NaturalLiteral{} -> return (Right e)
            IntegerLiteral{} -> return (Right e)
            BytesLiteral{} -> return (Right e)
            DateLiteral{} -> return (Right e)
            TimeLiteral{} -> return (Right e)
            TimeZoneLiteral{} -> return (Right e)
            Lambda x a b -> bin (Lambda x) a b
            Forall x a b -> bin (Forall x) a b
            Let x t a b -> do
                t' <- case t of
                    Nothing -> return (Right Nothing)
                    Just ty -> fmap Just <$> go ty
                a' <- go a
                b' <- go b
                return (Let x <$> t' <*> a' <*> b')
            If a b c -> tri If a b c
            Merge a b t -> do
                a' <- go a
                b' <- go b
                t' <- case t of
                    Nothing -> return (Right Nothing)
                    Just ty -> fmap Just <$> go ty
                return (Merge <$> a' <*> b' <*> t')
            ToMap a t -> do
                a' <- go a
                t' <- case t of
                    Nothing -> return (Right Nothing)
                    Just ty -> fmap Just <$> go ty
                return (ToMap <$> a' <*> t')
            EmptyList a -> fmap EmptyList <$> go a
            NonEmptyList xs -> do
                xs' <- fmap sequence (traverse go xs)
                return (NonEmptyList <$> xs')
            Annotation a b -> bin Annotation a b
            Operator a op b -> bin (\x y -> Operator x op y) a b
            Application a b -> bin Application a b
            Field a x -> fmap (\e' -> Field e' x) <$> go a
            ProjectByLabels a xs -> fmap (\e' -> ProjectByLabels e' xs) <$> go a
            ProjectByType a b -> bin ProjectByType a b
            Completion a b -> bin Completion a b
            Assert a -> fmap Assert <$> go a
            With a ks b -> bin (\x y -> With x ks y) a b
            TextLiteral (Chunks chunks z) -> do
                chunks' <- mapM (\(s, t) -> fmap (\expr -> (s, expr)) <$> go t) chunks
                return (fmap (\cs -> TextLiteral (Chunks cs z)) (sequence chunks'))
            RecordType fields ->
                fmap RecordType <$> traversePair fields
            RecordLiteral fields ->
                fmap RecordLiteral <$> traversePair fields
            UnionType alts -> do
                alts' <- mapM resolveAlt alts
                return (UnionType <$> sequence alts')
            ShowConstructor a -> fmap ShowConstructor <$> go a
            Some a -> fmap Some <$> go a
            Import{} -> go e

    bin f a b = do
        a' <- go a
        b' <- go b
        return (f <$> a' <*> b')

    tri f a b c = do
        a' <- go a
        b' <- go b
        c' <- go c
        return (f <$> a' <*> b' <*> c')

    traversePair fields = do
        fields' <- mapM (\(k, t) -> fmap (\e -> (k, e)) <$> go t) fields
        return (sequence fields')

    resolveAlt (k, Nothing) =
        return (Right (k, Nothing))
    resolveAlt (k, Just t) =
        fmap (\e -> (k, Just e)) <$> go t

newInsecureManager :: IO Manager
newInsecureManager =
    HTTP.newManager
        (HTTP.TLS.mkManagerSettings
            (Connection.TLSSettingsSimple
                { Connection.settingDisableCertificateValidation = Prelude.True
                , Connection.settingDisableSession = Prelude.False
                , Connection.settingUseServerName = Prelude.True
                })
            Nothing)
```
