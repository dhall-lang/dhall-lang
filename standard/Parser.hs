{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{-| This module translates @./dhall.abnf@ into a parser implemented using an
    LL parser combinator package

    This parser optimizes for exactly corresponding to the ABNF grammar, at the
    expense of efficiency
-}
module Parser where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..), guard, replicateM)
import Data.Functor (void)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Void (Void)
import Prelude hiding (takeWhile)
import Text.Megaparsec (MonadParsec, Parsec, satisfy, takeWhileP, try)
import Text.Megaparsec.Char (char)

import Syntax
    ( Builtin(..)
    , Constant(..)
    , Expression
    , Operator(..)
    , TextLiteral(..)
    )

import qualified Data.Char as Char
import qualified Data.Text as Text

newtype Parser a = Parser { unParser :: Parsec Void Text a }
    deriving
    ( Alternative
    , Applicative
    , Functor
    , Monad
    , MonadParsec Void Text
    , MonadPlus
    , Monoid
    , Semigroup
    )

instance a ~ Text => IsString (Parser a) where
    fromString x = Parser (fromString x)

between :: Char -> Char -> Char -> Bool
between lo hi c = lo <= c && c <= hi

takeWhile :: (Char -> Bool) -> Parser Text
takeWhile = takeWhileP Nothing

digitToNumber :: Char -> Int
digitToNumber c
    | '0' <= c && c <= '9' = 0x0 + Char.ord c - Char.ord '0'
    | 'A' <= c && c <= 'F' = 0xA + Char.ord c - Char.ord 'A'
    | 'a' <= c && c <= 'f' = 0xa + Char.ord c - Char.ord 'a'
    | otherwise = error "Invalid hexadecimal digit"

caseInsensitive :: Char -> Char -> Bool
caseInsensitive expected actual = Char.toUpper actual == expected

base :: [Char] -> Int -> Int
digits `base` b = foldl snoc 0 (map digitToNumber digits)
  where
    snoc result number = result * b + number

upTo :: Int -> Parser a -> Parser [a]
upTo 0 _ = do
    return []
upTo n parser = (do
    x <- parser

    xs <- upTo (n - 1) parser

    return (x : xs) ) <|> return []

range :: Int -> Int -> Parser a -> Parser [a]
range lowerBound upperBound parser = do
    prefix <- replicateM lowerBound parser

    suffix <- upTo (upperBound - lowerBound) parser

    return (prefix <> suffix)

endOfLine :: Parser Text
endOfLine = "\n" <|> "\r\n"

validNonAscii :: Char -> Bool
validNonAscii c =
       between     '\x80'   '\xD7FF' c
    || between   '\xE000'   '\xFFFD' c
    || between  '\x10000'  '\x1FFFD' c
    || between  '\x20000'  '\x2FFFD' c
    || between  '\x30000'  '\x3FFFD' c
    || between  '\x40000'  '\x4FFFD' c
    || between  '\x50000'  '\x5FFFD' c
    || between  '\x60000'  '\x6FFFD' c
    || between  '\x70000'  '\x7FFFD' c
    || between  '\x80000'  '\x8FFFD' c
    || between  '\x90000'  '\x9FFFD' c
    || between  '\xA0000'  '\xAFFFD' c
    || between  '\xB0000'  '\xBFFFD' c
    || between  '\xC0000'  '\xCFFFD' c
    || between  '\xD0000'  '\xDFFFD' c
    || between  '\xE0000'  '\xEFFFD' c
    || between  '\xF0000'  '\xFFFFD' c
    || between '\x100000' '\x10FFFD' c

tab :: Char
tab = '\t'

blockComment :: Parser ()
blockComment = do "{-"; blockCommentContinue

blockCommentChar :: Parser ()
blockCommentChar =
        void (satisfy (between '\x20' '\x7F'))
    <|> void (satisfy validNonAscii)
    <|> void (char tab)
    <|> void endOfLine
    
blockCommentContinue :: Parser ()
blockCommentContinue =
        void "-}"
    <|> (do blockComment; blockCommentContinue)
    <|> (do blockCommentChar; blockCommentContinue)

notEndOfLine :: Parser ()
notEndOfLine = void (satisfy predicate)
  where
    predicate c =
            between '\x20' '\x7F' c
        ||  validNonAscii c
        ||  tab == c

lineComment :: Parser ()
lineComment = do "--"; _ <- many notEndOfLine; _ <- endOfLine; return ()

whitespaceChunk :: Parser ()
whitespaceChunk =
        void " "
    <|> void (char tab)
    <|> void endOfLine
    <|> lineComment
    <|> blockComment

whsp :: Parser ()
whsp = void (many whitespaceChunk)

whsp1 :: Parser ()
whsp1 = void (some whitespaceChunk)

alpha :: Char -> Bool
alpha c = between '\x41' '\x5A' c || between '\x61' '\x7A' c

digit :: Char -> Bool
digit = between '\x30' '\x39'

alphaNum :: Char -> Bool
alphaNum c = alpha c || digit c

hexUpTo :: Char -> Char -> Bool
hexUpTo upperBound c = digit c || between 'A' upperBound (Char.toUpper c)

hexDig :: Char -> Bool
hexDig = hexUpTo 'F'

simpleLabelFirstChar :: Char -> Bool
simpleLabelFirstChar c = alpha c || c == '_'

simpleLabelNextChar :: Char -> Bool
simpleLabelNextChar c = alphaNum c || c `elem` [ '-', '/', '_' ]

simpleLabel :: Parser Text
simpleLabel = try do
    first <- satisfy simpleLabelFirstChar

    rest  <- takeWhile simpleLabelNextChar

    let l = Text.cons first rest

    guard (l `notElem` reservedKeywords)

    return l

quotedLabelChar :: Char -> Bool
quotedLabelChar c = between '\x20' '\x5F' c || between '\x61' '\x7E' c

quotedLabel :: Parser Text
quotedLabel = takeWhile quotedLabelChar

label :: Parser Text
label = (do "`"; l <- quotedLabel; "`"; return l)
    <|> simpleLabel

nonreservedLabels :: Parser Text
nonreservedLabels =
        (do "`"; l <- quotedLabel; "`"; guard (l `notElem` builtins); return l)
    <|> simpleLabel

anyLabel :: Parser Text
anyLabel = label

anyLabelOrSome :: Parser Text
anyLabelOrSome = anyLabel <|> "Some"

doubleQuoteChunk :: Parser TextLiteral
doubleQuoteChunk =
        interpolation
    <|> (do _ <- char '\x5C';

            c <- doubleQuoteEscaped;

            return (Chunks [] (Text.singleton c))
        )
    <|> (do c <- satisfy doubleQuoteChar

            return (Chunks [] (Text.singleton c))
        )

doubleQuoteEscaped :: Parser Char
doubleQuoteEscaped =
        char '\x22'
    <|> char '\x24'
    <|> char '\x5C'
    <|> char '\x2F'
    <|> char '\x62'
    <|> char '\x66'
    <|> char '\x6E'
    <|> char '\x72'
    <|> char '\x74'
    <|> (do char '\x75'; unicodeEscape)

unicodeEscape :: Parser Char
unicodeEscape = do
    number <- unbracedEscape <|> (do "{"; c <- bracedEscape; "}"; return c)

    return (Char.chr number)

unicodeSuffix :: Parser Int
unicodeSuffix = beginsWithoutF <|> beginsWithF
  where
    beginsWithoutF = do
        digit0 <- satisfy (hexUpTo 'E')

        digits1 <- replicateM 3 (satisfy hexDig)

        return ((digit0 : digits1) `base` 16)

    beginsWithF = do
        digit0 <- satisfy (caseInsensitive 'F')

        digits1 <- replicateM 2 (satisfy hexDig)

        digit2 <- satisfy (hexUpTo 'D')

        return ((digit0 : digits1 <> [ digit2 ]) `base` 16)

unbracedEscape :: Parser Int
unbracedEscape = beginsUpToC <|> beginsWithD <|> beginsWithE <|> beginsWithF
  where
    beginsUpToC = do
        digit0 <- satisfy (hexUpTo 'C')

        digits1 <- replicateM 3 (satisfy hexDig)

        return ((digit0 : digits1) `base` 16)

    beginsWithD = do
        digit0 <- satisfy (caseInsensitive 'D')

        digit1 <- satisfy (between '0' '7')

        digits2 <- replicateM 2 (satisfy hexDig)

        return ((digit0 : digit1 : digits2) `base` 16)

    beginsWithE = do
        digit0 <- satisfy (caseInsensitive 'E')

        digits1 <- replicateM 3 (satisfy hexDig)

        return ((digit0 : digits1) `base` 16)

    beginsWithF = do
        digit0 <- satisfy (caseInsensitive 'F')

        digits1 <- replicateM 2 (satisfy hexDig)

        digit2 <- satisfy (hexUpTo 'D')

        return ((digit0 : digits1 <> [ digit2 ]) `base` 16)

bracedCodepoint :: Parser Int
bracedCodepoint = planes1Through16 <|> unbracedEscape <|> threeDigits
  where
    planes1Through16 = do
        prefix <- fmap digitToNumber (satisfy hexDig) <|> (do "10"; return 16)

        suffix <- unicodeSuffix

        return (prefix * 0x10000 + suffix)

    threeDigits = do
        digits <- range 1 3 (satisfy hexDig)

        return (digits `base` 16)

bracedEscape :: Parser Int
bracedEscape = do
    _ <- takeWhile (== '0')

    bracedCodepoint

doubleQuoteChar :: Char -> Bool
doubleQuoteChar c = do
        between '\x20' '\x21' c
    ||  between '\x23' '\x5B' c
    ||  between '\x5D' '\x7F' c
    ||  validNonAscii c

doubleQuoteLiteral :: Parser TextLiteral
doubleQuoteLiteral = do
    _ <- char '"'

    chunks <- many doubleQuoteChunk

    _ <- char '"'

    return (mconcat chunks)

singleQuoteContinue :: Parser TextLiteral
singleQuoteContinue =
        (interpolation <> singleQuoteContinue)
    <|> (escapedQuotePair <> singleQuoteContinue)
    <|> (escapedInterpolation <> singleQuoteContinue)
    <|> (do _ <- "''"; return mempty)
    <|> (singleQuoteChar <> singleQuoteContinue)

escapedQuotePair :: Parser TextLiteral
escapedQuotePair = do
    _ <- "'''"

    return (Chunks [] "''")

escapedInterpolation :: Parser TextLiteral
escapedInterpolation = do
    _ <- "''${"

    return (Chunks [] "${")

singleQuoteChar :: Parser TextLiteral
singleQuoteChar =
        (do c <- satisfy predicate

            return (Chunks [] (Text.singleton c))
        )
    <|> (do t <- endOfLine

            return (Chunks [] t)
        )
  where
    predicate c =
            between '\x20' '\x7F' c
        ||  validNonAscii c
        ||  tab == c

singleQuoteLiteral :: Parser TextLiteral
singleQuoteLiteral = do
    _ <- "''"

    _ <- endOfLine

    singleQuoteContinue

interpolation :: Parser TextLiteral
interpolation = do
    _ <- "${"

    e <- completeExpression

    _ <- "}"

    return (Chunks [("", e)] "")

textLiteral :: Parser TextLiteral
textLiteral = doubleQuoteLiteral <|> singleQuoteLiteral

keyword :: Parser ()
keyword =
        if_
    <|> then_
    <|> else_
    <|> let_
    <|> in_
    <|> using
    <|> missing
    <|> assert
    <|> as
    <|> _Infinity
    <|> _NaN
    <|> merge
    <|> _Some
    <|> toMap
    <|> forallKeyword
    <|> with

if_ :: Parser ()
if_ = void "if"

then_ :: Parser ()
then_ = void "then"

else_ :: Parser ()
else_ = void "else"

let_ :: Parser ()
let_ = void "let"

in_ :: Parser ()
in_ = void "in"

as :: Parser ()
as = void "as"

using :: Parser ()
using = void "using"

merge :: Parser ()
merge = void "merge"

missing :: Parser ()
missing = void "missing"

_Infinity :: Parser ()
_Infinity = void "Infinity"

_NaN :: Parser ()
_NaN = void "NaN"

_Some :: Parser ()
_Some = void "Some"

toMap :: Parser ()
toMap = void "toMap"

assert :: Parser ()
assert = void "assert"

forallKeyword :: Parser ()
forallKeyword = void "forall"

forallSymbol :: Parser ()
forallSymbol = void "∀"

forall :: Parser ()
forall = forallSymbol <|> forallKeyword

with :: Parser ()
with = void "with"

builtin :: Parser Builtin
builtin =
        _NaturalFold
    <|> _NaturalBuild
    <|> _NaturalIsZero
    <|> _NaturalEven
    <|> _NaturalOdd
    <|> _NaturalToInteger
    <|> _NaturalShow
    <|> _IntegerToDouble
    <|> _IntegerShow
    <|> _IntegerNegate
    <|> _IntegerClamp
    <|> _NaturalSubtract
    <|> _DoubleShow
    <|> _ListBuild
    <|> _ListFold
    <|> _ListLength
    <|> _ListHead
    <|> _ListLast
    <|> _ListIndexed
    <|> _ListReverse
    <|> _TextShow
    <|> _TextReplace
    <|> _Bool
    <|> _True
    <|> _False
    <|> _Optional
    <|> _None
    <|> _Natural
    <|> _Integer
    <|> _Double
    <|> _Text
    <|> _List

_NaturalFold :: Parser Builtin
_NaturalFold = do _ <- "Natural/fold"; return NaturalFold

_NaturalBuild :: Parser Builtin
_NaturalBuild = do _ <- "Natural/build"; return NaturalBuild

_NaturalIsZero :: Parser Builtin
_NaturalIsZero = do _ <- "Natural/isZero"; return NaturalIsZero

_NaturalEven :: Parser Builtin
_NaturalEven = do _ <- "Natural/even"; return NaturalEven

_NaturalOdd :: Parser Builtin
_NaturalOdd = do _ <- "Natural/odd"; return NaturalOdd

_NaturalToInteger :: Parser Builtin
_NaturalToInteger = do _ <- "Natural/toInteger"; return NaturalToInteger

_NaturalShow :: Parser Builtin
_NaturalShow = do _ <- "Natural/show"; return NaturalShow

_IntegerToDouble :: Parser Builtin
_IntegerToDouble = do _ <- "Integer/toDouble"; return IntegerToDouble

_IntegerShow :: Parser Builtin
_IntegerShow = do _ <- "Integer/show"; return IntegerShow

_IntegerNegate :: Parser Builtin
_IntegerNegate = do _ <- "Integer/negate"; return IntegerNegate

_IntegerClamp :: Parser Builtin
_IntegerClamp = do _ <- "Integer/clamp"; return IntegerClamp

_NaturalSubtract :: Parser Builtin
_NaturalSubtract = do _ <- "Natural/subtract"; return NaturalSubtract

_DoubleShow :: Parser Builtin
_DoubleShow = do _ <- "Double/show"; return DoubleShow

_ListBuild :: Parser Builtin
_ListBuild = do _ <- "List/build"; return ListBuild

_ListFold :: Parser Builtin
_ListFold = do _ <- "List/fold"; return ListFold

_ListLength :: Parser Builtin
_ListLength = do _ <- "List/length"; return ListLength

_ListHead :: Parser Builtin
_ListHead = do _ <- "List/head"; return ListHead

_ListLast :: Parser Builtin
_ListLast = do _ <- "List/last"; return ListLast

_ListIndexed :: Parser Builtin
_ListIndexed = do _ <- "List/indexed"; return ListIndexed

_ListReverse :: Parser Builtin
_ListReverse = do _ <- "List/reverse"; return ListReverse

_TextShow :: Parser Builtin
_TextShow = do _ <- "Text/show"; return TextShow

_TextReplace :: Parser Builtin
_TextReplace = do _ <- "Text/replace"; return TextReplace

_Bool :: Parser Builtin
_Bool = do _ <- "Bool"; return Bool

_True :: Parser Builtin
_True = do _ <- "True"; return Syntax.True

_False :: Parser Builtin
_False = do _ <- "False"; return Syntax.False

_Optional :: Parser Builtin
_Optional = do _ <- "Optional"; return Optional

_None :: Parser Builtin
_None = do _ <- "None"; return None

_Natural :: Parser Builtin
_Natural = do _ <- "Natural"; return Natural

_Integer :: Parser Builtin
_Integer = do _ <- "Integer"; return Integer

_Double :: Parser Builtin
_Double = do _ <- "Double"; return Double

_Text :: Parser Builtin
_Text = do _ <- "Text"; return Text

_List :: Parser Builtin
_List = do _ <- "List"; return List

constant :: Parser Constant
constant =
        _Type
    <|> _Kind
    <|> _Sort

_Type :: Parser Constant
_Type = do _ <- "Type"; return Type

_Kind :: Parser Constant
_Kind = do _ <- "Kind"; return Kind

_Sort :: Parser Constant
_Sort = do _ <- "Sort"; return Sort

combine :: Parser Operator
combine = do _ <- "∧" <|> "/\\"; return CombineRecordTerms

combineTypes :: Parser Operator
combineTypes = do _ <- "⩓" <|> "//\\\\"; return CombineRecordTypes

equivalent :: Parser Operator
equivalent = do _ <- "≡" <|> "==="; return Equivalent

prefer :: Parser Operator
prefer = do _ <- "⫽" <|> "//"; return Prefer

lambda :: Parser ()
lambda = do _ <- "λ" <|> "\\"; return ()

arrow :: Parser ()
arrow = do _ <- "→" <|> "->"; return ()

complete :: Parser ()
complete = do _ <- "::"; return ()

completeExpression :: Parser Expression
completeExpression = undefined

reservedKeywords :: [Text]
reservedKeywords =
    [ "if"
    , "then"
    , "else"
    , "let"
    , "in"
    , "using"
    , "missing"
    , "assert"
    , "as"
    , "Infinity"
    , "NaN"
    , "merge"
    , "Some"
    , "toMap"
    , "forall"
    , "with"
    ]

builtins :: [Text]
builtins =
    [ "Natural/fold"
    , "Natural/build"
    , "Natural/isZero"
    , "Natural/even"
    , "Natural/odd"
    , "Natural/toInteger"
    , "Natural/show"
    , "Integer/toDouble"
    , "Integer/show"
    , "Integer/negate"
    , "Integer/clamp"
    , "Natural/subtract"
    , "Double/show"
    , "List/build"
    , "List/fold"
    , "List/length"
    , "List/head"
    , "List/last"
    , "List/indexed"
    , "List/reverse"
    , "Text/show"
    , "Text/replace"
    , "Bool"
    , "True"
    , "False"
    , "Optional"
    , "None"
    , "Natural"
    , "Integer"
    , "Double"
    , "Text"
    , "List"
    , "Type"
    , "Kind"
    , "Sort"
    ]
