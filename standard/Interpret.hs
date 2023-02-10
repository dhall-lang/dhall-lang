{-| This module serves three purposes:

    * … to show how to connect together the various interpretation phases into a
      complete interpretation pass

    * … to power the @dhall@ executable included in this package which is a
      reference implementation of the Dhall configuration language

    * … to help generating the CBOR-encoded Dhall files (*.dhallb) of the test
      suite. To do so, run @dhall output.dhallb < input.dhall@.
-}
module Interpret
    ( -- * Main
      main
    ) where

import qualified Binary
import qualified Codec.CBOR.Term    as CBOR
import qualified Codec.CBOR.Write   as CBOR
import qualified Data.ByteString    as ByteString
import qualified Data.Text.IO       as Text.IO
import qualified Parser
import qualified System.Environment
import qualified Text.Megaparsec    as Megaparsec

main :: IO ()
main = do
    input <- Text.IO.getContents

    let parser = Parser.unParser do
            e <- Parser.completeExpression

            Megaparsec.eof

            return e

    expression <- case Megaparsec.runParser parser "(input)" input of
       Left  errors     -> fail (Megaparsec.errorBundlePretty errors)
       Right expression -> return expression

    let encoded = Binary.encode expression

    -- Print a human readable representation of the CBOR terms of the encoded
    -- expression to stdout.
    print encoded

    -- Write the serialized CBOR terms to the file provided as the first command
    -- line argument (if there is one).
    args <- System.Environment.getArgs
    case args of
        [fp] -> ByteString.writeFile fp (CBOR.toStrictByteString (CBOR.encodeTerm encoded))
        _ -> return ()
