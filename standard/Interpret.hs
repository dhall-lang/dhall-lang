{-# LANGUAGE BlockArguments #-}

module Interpret where

import qualified Binary
import qualified Text.Earley as Earley
import qualified Parser

main :: IO ()
main = do
    input <- getContents

    let (expressions, report) =
            Earley.fullParses (Earley.parser Parser.grammar) input

    case expressions of
        expression : _ -> print (Binary.encode expression)
        _              -> print report
