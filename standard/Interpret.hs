{-| This module serves two purposes:

    * … to show how to connect together the various interpretation phases into a
      complete interpretation pass

    * … to power the @dhall@ executable included in this package which is a
      reference implementation of the Dhall configuration language
-}
module Interpret
    ( -- * Main
      main
    ) where

import qualified Binary
import qualified Data.Text.IO    as Text.IO
import qualified Parser
import qualified Text.Megaparsec as Megaparsec

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

    print (Binary.encode expression)
