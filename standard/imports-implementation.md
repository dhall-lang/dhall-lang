# Import resolution (reference implementation)

Literate Haskell for the algorithm in `imports-implementation-notes.md` and
the chaining / canonicalization / `as Location` judgments in `imports.md`.

```haskell
module Imports
    ( ImportError(..)
    , ResolveEnv(..)
    , newInsecureManager
    , newResolveEnv
    , resolveExpression
    , canonicalizeImport
    , chainImports
    ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Crypto.Hash (Digest, SHA256)
import Data.IORef (IORef)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Network.HTTP.Client (Manager)
import Prelude hiding (Bool(..))
import System.FilePath ((</>))
import Syntax

import qualified AlphaNormalization
import qualified BetaNormalization
import qualified Binary
import qualified Codec.CBOR.Read           as CBOR.Read
import qualified Codec.CBOR.Term           as CBOR.Term
import qualified Codec.CBOR.Write          as CBOR.Write
import qualified Crypto.Hash               as Hash
import qualified Data.ByteString           as ByteString
import qualified Data.ByteString.Lazy      as ByteString.Lazy
import qualified Data.CaseInsensitive      as CI
import qualified Data.IORef                as IORef
import qualified Data.List.NonEmpty        as NonEmpty
import qualified Data.Map                  as Map
import qualified Data.Maybe                as Maybe
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text.Encoding
import qualified Equivalence
import qualified Network.Connection        as Connection
import qualified Network.HTTP.Client       as HTTP
import qualified Network.HTTP.Client.TLS   as HTTP.TLS
import qualified Network.HTTP.Types        as HTTP.Types
import qualified Parser
import qualified Prelude
import qualified System.Directory          as Directory
import qualified System.Environment        as Environment
import qualified System.FilePath           as FilePath
import qualified Text.Megaparsec           as Megaparsec
import qualified TypeInference

data ImportError
    = SoftFailure String
    | HardFailure String
    deriving (Show)

-- | How to treat child imports of an @as Source@ walk.
data ChildPolicy
    = InlineEverything
    | PreserveHashed
    deriving (Eq)

data ResolveEnv = ResolveEnv
    { rootCwd          :: FilePath
    , homeDirectory    :: FilePath
    , stack            :: NonEmpty ImportType
    , httpManager      :: Manager
    , memoryByHash     :: IORef (Map (Digest SHA256) Expression)
    , memoryByKey      :: IORef (Map (Text, ImportMode) Expression)
    , originHeadersRef :: IORef (Maybe Expression)
    , cacheHome        :: FilePath
    , useSemanticCache :: Prelude.Bool
    }

newResolveEnv
    :: FilePath
    -> FilePath
    -> NonEmpty ImportType
    -> Manager
    -> FilePath
    -> Prelude.Bool
    -> IO ResolveEnv
newResolveEnv root home ancestor manager cache useDisk = do
    byHash <- IORef.newIORef Map.empty
    byKey  <- IORef.newIORef Map.empty
    origin <- IORef.newIORef Nothing
    return ResolveEnv
        { rootCwd          = root
        , homeDirectory    = home
        , stack            = ancestor
        , httpManager      = manager
        , memoryByHash     = byHash
        , memoryByKey      = byKey
        , originHeadersRef = origin
        , cacheHome        = cache
        , useSemanticCache = useDisk
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
            Nothing -> ""
            Just q  -> "?" <> q

renderOrigin :: URL -> Text
renderOrigin (URL scheme₀ authority₀ _ _) =
    schemeText <> "://" <> authority₀
  where
    schemeText =
        case scheme₀ of
            HTTP  -> "http"
            HTTPS -> "https"

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

isRemote :: ImportType -> Prelude.Bool
isRemote (Remote _ _) = Prelude.True
isRemote _            = Prelude.False

sameOrigin :: URL -> URL -> Prelude.Bool
sameOrigin parent child =
    scheme parent == scheme child && authority parent == authority child

corsCompliant :: ImportType -> ImportType -> [HTTP.Types.Header] -> Prelude.Bool
corsCompliant parent child headers =
    case (parent, child) of
        (_, Remote _ _) | not (isRemote parent) ->
            Prelude.True
        (Remote parentURL _, Remote childURL _)
            | sameOrigin parentURL childURL ->
                Prelude.True
            | otherwise ->
                case acaoValues of
                    [v]
                        | v == "*" ->
                            Prelude.True
                        | v == Text.Encoding.encodeUtf8 (renderOrigin parentURL) ->
                            Prelude.True
                        | otherwise ->
                            Prelude.False
                    _ ->
                        Prelude.False
        _ ->
            Prelude.True
  where
    acaoValues =
        [ value
        | (name, value) <- headers
        , CI.foldedCase name == "access-control-allow-origin"
        ]

textLit :: Text -> Expression
textLit t = TextLiteral (Chunks [] t)

asTextLiteral :: Expression -> Maybe Text
asTextLiteral (TextLiteral (Chunks [] t)) = Just t
asTextLiteral _                           = Nothing

fromMapEntry :: Expression -> Maybe (Text, Expression)
fromMapEntry (RecordLiteral fields) = do
    keyExpr <- lookup "mapKey" fields <|> lookup "header" fields
    valExpr <- lookup "mapValue" fields <|> lookup "value" fields
    key <- asTextLiteral keyExpr
    return (key, valExpr)
fromMapEntry _ =
    Nothing

fromDhallMap :: Expression -> Maybe [(Text, Expression)]
fromDhallMap (EmptyList _) = Just []
fromDhallMap (NonEmptyList xs) =
    traverse fromMapEntry (NonEmpty.toList xs)
fromDhallMap _ =
    Nothing

headerRecordType :: Expression
headerRecordType =
    RecordType [("mapKey", Builtin Text), ("mapValue", Builtin Text)]

headerListType :: Expression
headerListType =
    Application (Builtin List) headerRecordType

originHeadersType :: Expression
originHeadersType =
    Application (Builtin List)
        (RecordType
            [ ("mapKey", Builtin Text)
            , ("mapValue", headerListType)
            ]
        )

emptyOriginHeaders :: Expression
emptyOriginHeaders = EmptyList originHeadersType

encodeBytes :: Expression -> ByteString.ByteString
encodeBytes expression =
    CBOR.Write.toStrictByteString
        (CBOR.Term.encodeTerm (Binary.encode expression))

expressionHash :: Expression -> Digest SHA256
expressionHash expression =
    Hash.hash (encodeBytes expression)

decodeExpressionBytes :: ByteString.ByteString -> Maybe Expression
decodeExpressionBytes bytes = do
    term <- case CBOR.Read.deserialiseFromBytes CBOR.Term.decodeTerm (ByteString.Lazy.fromStrict bytes) of
        Right (_, t) -> Just t
        Left _       -> Nothing
    Binary.decode term

semanticCacheFile :: ResolveEnv -> Digest SHA256 -> FilePath
semanticCacheFile env digest =
    cacheHome env </> "dhall" </> ("1220" <> show digest)

lookupSemanticCache :: ResolveEnv -> Digest SHA256 -> IO (Maybe Expression)
lookupSemanticCache env digest = do
    mem <- IORef.readIORef (memoryByHash env)
    case Map.lookup digest mem of
        Just expression ->
            return (Just expression)
        Nothing
            | not (useSemanticCache env) ->
                return Nothing
            | otherwise -> do
                let cacheFile = semanticCacheFile env digest
                exists <- Directory.doesFileExist cacheFile
                if not exists
                    then return Nothing
                    else do
                        bytes <- ByteString.readFile cacheFile
                        let actual = Hash.hash bytes :: Digest SHA256
                        if actual /= digest
                            then return Nothing
                            else case decodeExpressionBytes bytes of
                                Nothing -> return Nothing
                                Just expression -> do
                                    IORef.modifyIORef' (memoryByHash env) (Map.insert digest expression)
                                    return (Just expression)

storeSemanticCache :: ResolveEnv -> Digest SHA256 -> Expression -> IO ()
storeSemanticCache env digest expression = do
    IORef.modifyIORef' (memoryByHash env) (Map.insert digest expression)
    if not (useSemanticCache env)
        then return ()
        else do
            let cacheFile = semanticCacheFile env digest
            Directory.createDirectoryIfMissing Prelude.True (cacheHome env </> "dhall")
            ByteString.writeFile cacheFile (encodeBytes expression)

lookupMemoryKey :: ResolveEnv -> ImportType -> ImportMode -> IO (Maybe Expression)
lookupMemoryKey env child mode = do
    mem <- IORef.readIORef (memoryByKey env)
    return (Map.lookup (prettyImportType child, mode) mem)

storeMemoryKey :: ResolveEnv -> ImportType -> ImportMode -> Expression -> IO ()
storeMemoryKey env child mode expression =
    IORef.modifyIORef' (memoryByKey env)
        (Map.insert (prettyImportType child, mode) expression)

checkHash :: Maybe (Digest SHA256) -> Expression -> Either ImportError ()
checkHash Nothing _ =
    Right ()
checkHash (Just expected) expression =
    let actual = expressionHash expression
    in  if actual == expected
            then Right ()
            else Left (HardFailure "hash mismatch")

cacheProduct :: ImportMode -> Expression -> Expression
cacheProduct Code expression =
    AlphaNormalization.alphaNormalize expression
cacheProduct _ expression =
    expression

applyRequestHeaders :: HTTP.Request -> [(Text, Text)] -> HTTP.Request
applyRequestHeaders request headers =
    request { HTTP.requestHeaders = kept ++ encoded }
  where
    names = map (\(k, _) -> CI.mk (Text.Encoding.encodeUtf8 k)) headers
    kept =
        filter (\(name, _) -> name `notElem` names) (HTTP.requestHeaders request)
    encoded =
        [ (CI.mk (Text.Encoding.encodeUtf8 k), Text.Encoding.encodeUtf8 v)
        | (k, v) <- headers
        ]

fetchHTTP
    :: Manager
    -> URL
    -> [(Text, Text)]
    -> IO (Either ImportError (ByteString.ByteString, [HTTP.Types.Header]))
fetchHTTP manager url headers = do
    request <- HTTP.parseRequest (Text.unpack (renderURL url))
    let request' = applyRequestHeaders request headers
    result <- try (HTTP.httpLbs request' manager)
        :: IO (Either SomeException (HTTP.Response ByteString.Lazy.ByteString))
    case result of
        Left exception ->
            return (Left (SoftFailure (show exception)))
        Right response ->
            let status = HTTP.Types.statusCode (HTTP.responseStatus response)
            in  if status >= 200 && status < 300
                    then
                        return
                            (Right
                                ( ByteString.Lazy.toStrict (HTTP.responseBody response)
                                , HTTP.responseHeaders response
                                )
                            )
                    else
                        return (Left (SoftFailure ("HTTP " <> show status)))

readLocalOrEnv
    :: ResolveEnv
    -> ImportType
    -> IO (Either ImportError ByteString.ByteString)
readLocalOrEnv _ Missing =
    return (Left (SoftFailure "missing"))
readLocalOrEnv _ (Env name) = do
    mValue <- Environment.lookupEnv (Text.unpack name)
    case mValue of
        Nothing ->
            return (Left (SoftFailure ("unset environment variable: " <> Text.unpack name)))
        Just value ->
            return (Right (Text.Encoding.encodeUtf8 (Text.pack value)))
readLocalOrEnv env (Path prefix file₀) = do
    filePath <- localFilePath env prefix file₀
    exists <- Directory.doesFileExist filePath
    if not exists
        then return (Left (SoftFailure ("missing file: " <> filePath)))
        else Right <$> ByteString.readFile filePath
readLocalOrEnv _ Remote{} =
    return (Left (HardFailure "internal: remote fetch requires headers"))

headerExprTypechecks :: Expression -> Expression -> Prelude.Bool
headerExprTypechecks expected expression =
    case TypeInference.inferType [] expression of
        Nothing -> Prelude.False
        Just inferred -> Equivalence.equivalent inferred expected

extractRequestHeaders :: Expression -> Maybe [(Text, Text)]
extractRequestHeaders expression = do
    entries <- fromDhallMap expression
    traverse (\(k, v) -> (,) k <$> asTextLiteral v) entries

extractOriginHeaders :: Expression -> Maybe [(Text, [(Text, Text)])]
extractOriginHeaders expression = do
    entries <- fromDhallMap expression
    traverse (\(k, v) -> (,) k <$> extractRequestHeaders v) entries

resolveHeadersExpr
    :: ResolveEnv
    -> Expression
    -> Expression
    -> IO (Either ImportError Expression)
resolveHeadersExpr env expectedType expression = do
    resolved <- walkExpression env InlineEverything expression
    case resolved of
        Left err ->
            return (Left err)
        Right value
            | headerExprTypechecks expectedType value ->
                return (Right (BetaNormalization.betaNormalize value))
            | otherwise ->
                return (Left (HardFailure "headers expression has the wrong type"))

configHeadersPath :: ResolveEnv -> IO ImportType
configHeadersPath env = do
    xdg <- Environment.lookupEnv "XDG_CONFIG_HOME"
    let configDir = Maybe.fromMaybe (homeDirectory env </> ".config") xdg
    let full = configDir </> "dhall" </> "headers.dhall"
    let parts =
            filter (`notElem` [".", "/", "\\"])
                (map Text.pack (FilePath.splitDirectories full))
    return $
        case reverse parts of
            fileName : dirRev ->
                Path Absolute (File dirRev fileName)
            [] ->
                Path Absolute (File [] "headers.dhall")

loadOriginHeaders :: ResolveEnv -> IO (Either ImportError Expression)
loadOriginHeaders env = do
    cached <- IORef.readIORef (originHeadersRef env)
    case cached of
        Just expression ->
            return (Right expression)
        Nothing -> do
            configPath <- configHeadersPath env
            let fallback = emptyOriginHeaders
            let expr =
                    Operator
                        (Import (Env "DHALL_HEADERS") Code Nothing)
                        Alternative
                        (Operator
                            (Import configPath Code Nothing)
                            Alternative
                            fallback
                        )
            result <- walkExpression env InlineEverything expr
            case result of
                Left err ->
                    return (Left err)
                Right headers -> do
                    IORef.writeIORef (originHeadersRef env) (Just headers)
                    return (Right headers)

originKeyHeaders :: Expression -> URL -> [(Text, Text)]
originKeyHeaders expression url =
    case extractOriginHeaders (BetaNormalization.betaNormalize expression) of
        Nothing -> []
        Just entries ->
            let keys =
                    [ authority url
                    , renderOrigin url
                    ]
            in  Maybe.fromMaybe [] (Maybe.listToMaybe [ v | k <- keys, (key, v) <- entries, key == k ])

mergeHeaders :: [(Text, Text)] -> [(Text, Text)] -> [(Text, Text)]
mergeHeaders origin inline =
    Map.toList (Map.union (Map.fromList origin) (Map.fromList inline))

requestHeadersFor
    :: ResolveEnv
    -> URL
    -> Maybe Expression
    -> IO (Either ImportError [(Text, Text)])
requestHeadersFor env url usingExpr = do
    loaded <- loadOriginHeaders env
    case loaded of
        Left err ->
            return (Left err)
        Right originExpr -> do
            let origin = originKeyHeaders originExpr url
            case usingExpr of
                Nothing ->
                    return (Right origin)
                Just expression -> do
                    resolved <- resolveHeadersExpr env headerListType expression
                    case resolved of
                        Left err ->
                            return (Left err)
                        Right value ->
                            case extractRequestHeaders value of
                                Nothing ->
                                    return (Left (HardFailure "could not extract request headers"))
                                Just inline ->
                                    return (Right (mergeHeaders origin inline))

fetchBytes
    :: ResolveEnv
    -> ImportType
    -> IO (Either ImportError ByteString.ByteString)
fetchBytes env child@(Remote url usingExpr) = do
    hdrs <- requestHeadersFor env url usingExpr
    case hdrs of
        Left err ->
            return (Left err)
        Right headers -> do
            fetched <- fetchHTTP (httpManager env) url headers
            case fetched of
                Left err ->
                    return (Left err)
                Right (body, responseHeaders) ->
                    if corsCompliant (NonEmpty.head (stack env)) child responseHeaders
                        then return (Right body)
                        else return (Left (HardFailure "CORS"))
fetchBytes env child =
    readLocalOrEnv env child

interpretBytes
    :: ResolveEnv
    -> ImportType
    -> ImportMode
    -> ByteString.ByteString
    -> IO (Either ImportError Expression)
interpretBytes _ _ RawBytes raw =
    return (Right (BytesLiteral raw))
interpretBytes _ _ RawText raw =
    case Text.Encoding.decodeUtf8' raw of
        Left exception ->
            return (Left (HardFailure (show exception)))
        Right text ->
            return (Right (textLit text))
interpretBytes _ child Location _ =
    return (Right (asLocation child))
interpretBytes env child mode raw = do
    text <- case Text.Encoding.decodeUtf8' raw of
        Left exception ->
            return (Left (HardFailure (show exception)))
        Right t ->
            return (Right t)
    case text of
        Left err ->
            return (Left err)
        Right src ->
            case parseExpression (Text.unpack (prettyImportType child)) src of
                Left errors ->
                    return (Left (HardFailure errors))
                Right parsed -> do
                    let env' = env{ stack = child :| NonEmpty.toList (stack env) }
                    let policy =
                            case mode of
                                Source -> PreserveHashed
                                _      -> InlineEverything
                    resolved <- walkExpression env' policy parsed
                    case resolved of
                        Left err ->
                            return (Left err)
                        Right expression ->
                            case mode of
                                Code ->
                                    case TypeInference.inferType [] expression of
                                        Nothing ->
                                            return (Left (HardFailure "imported expression is ill-typed"))
                                        Just _ ->
                                            return (Right (BetaNormalization.betaNormalize expression))
                                _ ->
                                    return (Right expression)

finalizeImport
    :: ResolveEnv
    -> ImportType
    -> ImportMode
    -> Maybe (Digest SHA256)
    -> Expression
    -> IO (Either ImportError Expression)
finalizeImport env child mode hash runtimeValue =
    let stored = cacheProduct mode runtimeValue
    in  case checkHash hash stored of
            Left err ->
                return (Left err)
            Right () -> do
                case hash of
                    Just digest -> storeSemanticCache env digest stored
                    Nothing     -> return ()
                value <- case mode of
                    Source -> do
                        let env' = env{ stack = child :| NonEmpty.toList (stack env) }
                        expanded <- walkExpression env' InlineEverything runtimeValue
                        case expanded of
                            Left err ->
                                return (Left err)
                            Right expression ->
                                case TypeInference.inferType [] expression of
                                    Nothing ->
                                        return (Left (HardFailure "imported expression is ill-typed"))
                                    Just _ ->
                                        return (Right expression)
                    _ ->
                        return (Right runtimeValue)
                case value of
                    Left err ->
                        return (Left err)
                    Right expression -> do
                        case hash of
                            Nothing -> storeMemoryKey env child mode expression
                            Just _  -> return ()
                        return (Right expression)

loadImport
    :: ResolveEnv
    -> ImportType
    -> ImportMode
    -> Maybe (Digest SHA256)
    -> IO (Either ImportError Expression)
loadImport env raw mode hash = do
    let parent = NonEmpty.head (stack env)
    let child  = chainImports parent raw

    case mode of
        Location ->
            return (Right (asLocation child))
        _ ->
            loadFetched env child mode hash

loadFetched
    :: ResolveEnv
    -> ImportType
    -> ImportMode
    -> Maybe (Digest SHA256)
    -> IO (Either ImportError Expression)
loadFetched env child mode hash
    | onStack child (stack env) =
        return (Left (HardFailure ("cyclic import: " <> Text.unpack (prettyImportType child))))
    | not (referentiallySane (NonEmpty.head (stack env)) child) =
        return (Left (HardFailure "referential sanity"))
    | otherwise = do
        cachedProduct <- case hash of
            Just digest -> lookupSemanticCache env digest
            Nothing     -> return Nothing
        case cachedProduct of
            Just artifact ->
                finalizeImport env child mode hash artifact
            Nothing -> do
                keyed <-
                    case hash of
                        Just _  -> return Nothing
                        Nothing -> lookupMemoryKey env child mode
                case keyed of
                    Just expression ->
                        return (Right expression)
                    Nothing -> do
                        bytes <- fetchBytes env child
                        case bytes of
                            Left err ->
                                return (Left err)
                            Right raw -> do
                                interpreted <- interpretBytes env child mode raw
                                case interpreted of
                                    Left err ->
                                        return (Left err)
                                    Right artifact ->
                                        finalizeImport env child mode hash artifact

walkExpression
    :: ResolveEnv
    -> ChildPolicy
    -> Expression
    -> IO (Either ImportError Expression)
walkExpression env policy expression =
    case expression of
        Import importType mode hash -> do
            result <- loadImport env importType mode hash
            case result of
                Right _
                    | PreserveHashed <- policy
                    , Just _ <- hash -> do
                        let parent = NonEmpty.head (stack env)
                        let child  = chainImports parent importType
                        return (Right (Import child mode hash))
                other ->
                    return other
        Operator left Alternative right -> do
            leftResult <- walkExpression env policy left
            case leftResult of
                Right resolved ->
                    return (Right resolved)
                Left (SoftFailure _) ->
                    walkExpression env policy right
                Left err ->
                    return (Left err)
        other ->
            walk other
  where
    go = walkExpression env policy

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

resolveExpression
    :: ResolveEnv
    -> Expression
    -> IO (Either ImportError Expression)
resolveExpression env =
    walkExpression env InlineEverything

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
