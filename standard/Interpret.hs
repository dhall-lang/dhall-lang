module Interpret where

import qualified Binary
import qualified Data.Text.IO    as Text.IO
import qualified Parser
import qualified Text.Megaparsec as Megaparsec

main :: IO ()
main = do
    input <- Text.IO.getContents

    expression <- case Megaparsec.runParser (Parser.unParser Parser.completeExpression) "(input)" input of
       Left  errors     -> fail (Megaparsec.errorBundlePretty errors)
       Right expression -> return expression

    print (Binary.encode expression)
