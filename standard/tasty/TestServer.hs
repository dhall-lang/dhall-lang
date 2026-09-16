{-# LANGUAGE OverloadedStrings #-}

{-| HTTP(S) fixture server for import tests.

    Vendored from dhall-haskell's @dhall-test-server@ (the only allowed copy
    from that repository). Fixture lookup serves @tests/import/...@ from this
    repository, not @dhall/dhall-lang/tests/import@.
-}
module TestServer
    ( withServers
    ) where

import Control.Exception        (bracket, throwIO)
import Data.IORef               (IORef, atomicModifyIORef', newIORef)
import Network.HTTP.Types       (hContentType, hUserAgent, methodGet, status200, status403, status404)
import Network.Wai              (Application, Request (..), responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setHost, setPort)
import Network.Wai.Handler.WarpTLS  (runTLS, tlsSettings)
import System.Directory         (doesFileExist)
import System.FilePath          ((</>))
import System.IO.Error          (mkIOError, userErrorType)

import qualified Control.Concurrent       as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Data.ByteString.Char8    as BS8
import qualified Data.ByteString.Lazy     as ByteString.Lazy
import qualified Data.Maybe               as Maybe
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import qualified Network.Wai              as Wai
import qualified System.FilePath          as FilePath

httpPort :: Int
httpPort = 18080

httpsPort :: Int
httpsPort = 18443

certCandidates :: [FilePath]
certCandidates =
    [ "test-server/cert/cert.pem"
    , "standard/test-server/cert/cert.pem"
    ]

keyCandidates :: [FilePath]
keyCandidates =
    [ "test-server/cert/key.pem"
    , "standard/test-server/cert/key.pem"
    ]

-- | Run the local HTTP and HTTPS servers while @action@ executes.
-- The @testsDir@ argument is the acceptance-test root (the same path as
-- @testsRoot@ in 'Main').
withServers :: FilePath -> IO a -> IO a
withServers testsDir action = do
    (actualCertPath, actualKeyPath) <- resolveCertAndKeyPaths

    bracket (start actualCertPath actualKeyPath) stop (const action)
  where
    start actualCertPath actualKeyPath = do
        randomCounter <- newIORef 0
        httpReady <- Concurrent.newEmptyMVar
        httpsReady <- Concurrent.newEmptyMVar

        httpThread <- Async.async (runHttpServer httpReady (testHttpApp testsDir randomCounter))
        httpsThread <- Async.async (runHttpsServer actualCertPath actualKeyPath httpsReady (testHttpApp testsDir randomCounter))

        Concurrent.takeMVar httpReady
        Concurrent.takeMVar httpsReady

        pure (httpThread, httpsThread)

    stop (httpThread, httpsThread) = do
        Async.cancel httpThread
        Async.cancel httpsThread

runHttpServer :: Concurrent.MVar () -> Application -> IO ()
runHttpServer ready app =
    runSettings
        (setBeforeMainLoop (Concurrent.putMVar ready ()) (setHost "127.0.0.1" (setPort httpPort defaultSettings)))
        app

runHttpsServer :: FilePath -> FilePath -> Concurrent.MVar () -> Application -> IO ()
runHttpsServer actualCertPath actualKeyPath ready app =
    runTLS
        (tlsSettings actualCertPath actualKeyPath)
        (setBeforeMainLoop (Concurrent.putMVar ready ()) (setHost "127.0.0.1" (setPort httpsPort defaultSettings)))
        app

resolveCertAndKeyPaths :: IO (FilePath, FilePath)
resolveCertAndKeyPaths = do
    mCert <- firstExistingPath certCandidates
    mKey  <- firstExistingPath keyCandidates

    case (mCert, mKey) of
        (Just cert, Just key) ->
            return (cert, key)
        _ ->
            throwIO (mkIOError userErrorType "Missing TLS certificate files under test-server/cert" Nothing Nothing)

firstExistingPath :: [FilePath] -> IO (Maybe FilePath)
firstExistingPath [] = return Nothing
firstExistingPath (path : paths) = do
    exists <- doesFileExist path

    if exists then return (Just path) else firstExistingPath paths

testHttpApp :: FilePath -> IORef Int -> Application
testHttpApp testsDir randomCounter request respond = do
    mResponse <- responseFromTestFixtures testsDir request

    case mResponse of
        Just response -> respond response
        Nothing ->
            case pathInfo request of
                ["user-agent"] | isGet -> do
                    let userAgent = lookup hUserAgent (requestHeaders request)
                    respond (responseLBS status200 [(hContentType, "application/json")] (ByteString.Lazy.fromStrict (userAgentResponse userAgent)))

                ["random-string"] | isGet -> do
                    n <- nextRandomCounter randomCounter
                    let body = "dhall-test-random-string-" <> BS8.pack (show n) <> "\n"
                    respond (responseLBS status200 [(hContentType, "text/plain")] (ByteString.Lazy.fromStrict body))

                ["foo"] | isGet ->
                    if hasExampleTestHeader request then respond (dhallText "./bar") else respond response403

                ["bar"] | isGet ->
                    if hasExampleTestHeader request then respond (dhallText "True") else respond response403

                ["cors", "AllowedAll.dhall"] | isGet ->
                    respond (corsText (Just "*") "42")

                ["cors", "OnlySelf.dhall"] | isGet ->
                    respond (corsText (Just "https://127.0.0.1:18443") "42")

                ["cors", "OnlyOther.dhall"] | isGet ->
                    respond (corsText (Just "https://localhost:28080") "42")

                ["cors", "OnlyGithub.dhall"] | isGet ->
                    respond (corsText (Just "https://localhost:18443") "42")

                ["cors", "Empty.dhall"] | isGet ->
                    respond (corsText (Just "") "42")

                ["cors", "NoCORS.dhall"] | isGet ->
                    respond (corsText Nothing "42")

                ["cors", "Null.dhall"] | isGet ->
                    respond (corsText (Just "null") "42")

                ["cors", "SelfImportAbsolute.dhall"] | isGet ->
                    respond (corsText (Just "*") "https://127.0.0.1:18443/cors/NoCORS.dhall")

                ["cors", "SelfImportRelative.dhall"] | isGet ->
                    respond (corsText (Just "*") "./NoCORS.dhall")

                ["cors", "TwoHopsFail.dhall"] | isGet ->
                    respond (corsText (Just "*") "https://localhost:18443/tests/import/data/cors/OnlySelf.dhall")

                ["cors", "TwoHopsSuccess.dhall"] | isGet ->
                    respond (corsText (Just "*") "https://localhost:18443/tests/import/data/cors/OnlyGithub.dhall")

                _ -> respond response404
  where
    isGet = requestMethod request == methodGet

-- | @GET /tests/import/...@ serves @testsDir/import/...@ with CORS @*@.
responseFromTestFixtures :: FilePath -> Request -> IO (Maybe Wai.Response)
responseFromTestFixtures testsDir request
    | requestMethod request /= methodGet = return Nothing
    | otherwise =
        case pathInfo request of
            ("tests" : "import" : rest) -> do
                let shortPath =
                        FilePath.joinPath (fmap Text.unpack rest)

                mResponse <- serveTestFixture (testsDir </> "import" </> shortPath)

                return (Just (Maybe.fromMaybe response404 mResponse))
            _ ->
                return Nothing

serveTestFixture :: FilePath -> IO (Maybe Wai.Response)
serveTestFixture path = do
    exists <- doesFileExist path

    if not exists
        then return Nothing
        else do
            body <- normalizeWindowsLineEndings <$> BS8.readFile path

            return (Just (corsText (Just "*") body))
  where
    normalizeWindowsLineEndings =
        BS8.intercalate "\n" . fmap stripTrailingCarriageReturn . BS8.split '\n'

    stripTrailingCarriageReturn line
        | not (BS8.null line) && BS8.last line == '\r' = BS8.init line
        | otherwise = line

nextRandomCounter :: IORef Int -> IO Int
nextRandomCounter ref =
    atomicModifyIORef' ref (\n -> let n' = n + 1 in (n', n'))

userAgentResponse :: Maybe BS8.ByteString -> BS8.ByteString
userAgentResponse mUserAgent =
    "{\n  \"user-agent\": \"" <> agent <> "\"\n}\n"
  where
    agent = Maybe.fromMaybe "none_given" mUserAgent

hasExampleTestHeader :: Request -> Bool
hasExampleTestHeader request =
    fmap Text.toLower (decodeHeader =<< lookup "Test" (requestHeaders request)) == Just "example"
  where
    decodeHeader = either (const Nothing) Just . Text.decodeUtf8'

corsText :: Maybe BS8.ByteString -> BS8.ByteString -> Wai.Response
corsText mOrigin body =
    responseLBS status200 headers (ByteString.Lazy.fromStrict body)
  where
    baseHeaders = [(hContentType, "text/plain")]
    headers =
        case mOrigin of
            Nothing     -> baseHeaders
            Just origin -> ("Access-Control-Allow-Origin", origin) : baseHeaders

dhallText :: BS8.ByteString -> Wai.Response
dhallText = responseLBS status200 [(hContentType, "text/plain")] . ByteString.Lazy.fromStrict

response403 :: Wai.Response
response403 = responseLBS status403 [(hContentType, "text/plain")] "Forbidden\n"

response404 :: Wai.Response
response404 = responseLBS status404 [(hContentType, "text/plain")] "Not Found\n"
