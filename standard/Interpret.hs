{-| Reference Dhall interpreter.

    Pipeline (default):

    @
    stdin → parse → resolve imports → inferType → betaNormalize
    @

    Prints the CBOR 'Codec.CBOR.Term.Term' of the result.  If type-checking
    fails, the process exits non-zero.

    Flags:

    * @--parse-only@ — skip import resolution, type-checking, and normalization
      (used to generate parser @*.dhallb@ / @*.diag@ fixtures)
    * @--from-cbor@ — treat stdin as raw CBOR bytes (a Term), not Dhall text
    * @--diag@ — write RFC 8949 diagnostic notation instead of a Haskell Term
      / CBOR bytes

    Usage:

    @
    dhall [--parse-only] [--from-cbor] [--diag] [outfile]
    @

    If @outfile@ is supplied, CBOR bytes (or diagnostic text with @--diag@) are
    written there.  Relative imports are resolved against the current working
    directory.
-}
module Interpret
    ( -- * Main
      main
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import System.FilePath ((</>))

import qualified BetaNormalization
import qualified Binary
import qualified Codec.CBOR.Read        as CBOR.Read
import qualified Codec.CBOR.Term        as CBOR.Term
import qualified Codec.CBOR.Write       as CBOR.Write
import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Lazy   as ByteString.Lazy
import qualified Data.Text.IO           as Text.IO
import qualified Imports
import qualified Parser
import qualified System.Directory       as Directory
import qualified System.Environment     as Environment
import qualified System.Exit            as Exit
import qualified System.IO              as IO
import qualified Text.Megaparsec        as Megaparsec
import qualified TypeInference
import qualified Syntax

data Options = Options
    { parseOnly :: Bool
    , fromCbor  :: Bool
    , useDiag   :: Bool
    , output    :: Maybe FilePath
    }

parseOptions :: [String] -> Options
parseOptions = go Options
    { parseOnly = False
    , fromCbor  = False
    , useDiag   = False
    , output    = Nothing
    }
  where
    go opts [] = opts
    go opts ("--parse-only" : rest) =
        go opts{ parseOnly = True } rest
    go opts ("--from-cbor" : rest) =
        go opts{ fromCbor = True } rest
    go opts ("--diag" : rest) =
        go opts{ useDiag = True } rest
    go opts (path : rest) =
        go opts{ output = Just path } rest

readTerm :: ByteString.ByteString -> IO CBOR.Term.Term
readTerm bytes =
    case CBOR.Read.deserialiseFromBytes CBOR.Term.decodeTerm (ByteString.Lazy.fromStrict bytes) of
        Left err ->
            fail (show err)
        Right (_, term) ->
            return term

parseDhall :: FilePath -> Text -> IO Syntax.Expression
parseDhall sourcePath input = do
    let parser = Parser.unParser do
            e <- Parser.completeExpression
            Megaparsec.eof
            return e

    case Megaparsec.runParser parser sourcePath input of
        Left  errors     -> fail (Megaparsec.errorBundlePretty errors)
        Right expression -> return expression

interpretExpression :: Syntax.Expression -> IO Syntax.Expression
interpretExpression parsed = do
    cwd  <- Directory.getCurrentDirectory
    home <- Directory.getHomeDirectory
    cacheHome <- do
        xdg <- Environment.lookupEnv "XDG_CACHE_HOME"
        case xdg of
            Just path -> return path
            Nothing   -> return (home </> ".cache")
    manager <- Imports.newInsecureManager
    let ancestor = Syntax.Path Syntax.Here (Syntax.File [] "stdin")
    env <- Imports.newResolveEnv cwd home (ancestor :| []) manager cacheHome True
    resolved <- Imports.resolveExpression env parsed
    expression <- case resolved of
        Left err ->
            fail (show err)
        Right value ->
            return value
    case TypeInference.inferType [] expression of
        Nothing -> do
            IO.hPutStrLn IO.stderr "Type inference failed"
            Exit.exitFailure
        Just _ ->
            return (BetaNormalization.betaNormalize expression)

emit :: Options -> CBOR.Term.Term -> IO ()
emit options term = do
    let diagnostic = Binary.diag term
    if useDiag options
        then do
            Text.IO.putStrLn diagnostic
            case output options of
                Just path -> Text.IO.writeFile path (diagnostic <> "\n")
                Nothing   -> return ()
        else do
            print term
            case output options of
                Just path ->
                    ByteString.writeFile path
                        (CBOR.Write.toStrictByteString (CBOR.Term.encodeTerm term))
                Nothing ->
                    return ()

main :: IO ()
main = do
    options <- parseOptions <$> Environment.getArgs

    term <-
        if fromCbor options
            then do
                bytes <- ByteString.getContents
                readTerm bytes
            else do
                input <- Text.IO.getContents
                parsed <- parseDhall "(input)" input
                expression <-
                    if parseOnly options
                        then return parsed
                        else interpretExpression parsed
                return (Binary.encode expression)

    emit options term
