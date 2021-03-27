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

import Control.Applicative (Alternative(..), optional)
import Control.Monad (MonadPlus(..), guard, replicateM)
import Crypto.Hash (Digest, SHA256)
import Data.ByteArray.Encoding (Base(..))
import Data.ByteString (ByteString)
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty(..))
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Void (Void)
import Numeric.Natural (Natural)
import Prelude hiding (exponent, takeWhile)
import Text.Megaparsec.Char (char)

import Syntax
    ( Builtin(..)
    , Constant(..)
    , Expression(..)
    , File(..)
    , FilePrefix(..)
    , ImportMode(..)
    , ImportType(..)
    , Operator(..)
    , Scheme(..)
    , TextLiteral(..)
    , URL(..)
    )
import Text.Megaparsec
    ( MonadParsec
    , Parsec
    , satisfy
    , takeWhileP
    , takeWhile1P
    , try
    )

import qualified Control.Monad.Combinators.NonEmpty as Combinators.NonEmpty
import qualified Crypto.Hash                        as Hash
import qualified Data.ByteArray.Encoding            as ByteArray.Encoding
import qualified Data.Char                          as Char
import qualified Data.List.NonEmpty                 as NonEmpty
import qualified Data.Text                          as Text
import qualified Data.Text.Encoding                 as Text.Encoding

newtype Parser a = Parser { unParser :: Parsec Void Text a }
    deriving
    ( Alternative
    , Applicative
    , Functor
    , Monad
    , MonadFail
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

takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 = takeWhile1P Nothing

digitToNumber :: Char -> Int
digitToNumber c
    | '0' <= c && c <= '9' = 0x0 + Char.ord c - Char.ord '0'
    | 'A' <= c && c <= 'F' = 0xA + Char.ord c - Char.ord 'A'
    | 'a' <= c && c <= 'f' = 0xa + Char.ord c - Char.ord 'a'
    | otherwise = error "Invalid hexadecimal digit"

caseInsensitive :: Char -> Char -> Bool
caseInsensitive expected actual = Char.toUpper actual == expected

base :: Num n => [Char] -> n -> n
digits `base` b = foldl snoc 0 (map (fromIntegral . digitToNumber) digits)
  where
    snoc result number = result * b + number

atMost :: Int -> Parser a -> Parser [a]
atMost 0 _ = do
    return []
atMost n parser = (do
    x <- parser

    xs <- atMost (n - 1) parser

    return (x : xs) ) <|> return []

atLeast :: Int -> Parser a -> Parser [a]
atLeast lowerBound parser = do
    prefix <- replicateM lowerBound parser

    suffix <- many parser

    return (prefix <> suffix)

range :: Int -> Int -> Parser a -> Parser [a]
range lowerBound upperBound parser = do
    prefix <- replicateM lowerBound parser

    suffix <- atMost (upperBound - lowerBound) parser

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
lineComment = do "--"; many notEndOfLine; endOfLine; return ()

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

nonreservedLabel :: Parser Text
nonreservedLabel =
        (do "`"; l <- quotedLabel; "`"; guard (l `notElem` builtins); return l)
    <|> simpleLabel

anyLabel :: Parser Text
anyLabel = label

anyLabelOrSome :: Parser Text
anyLabelOrSome = anyLabel <|> "Some"

doubleQuoteChunk :: Parser TextLiteral
doubleQuoteChunk =
        interpolation
    <|> (do char '\x5C';

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
    takeWhile (== '0')

    bracedCodepoint

doubleQuoteChar :: Char -> Bool
doubleQuoteChar c = do
        between '\x20' '\x21' c
    ||  between '\x23' '\x5B' c
    ||  between '\x5D' '\x7F' c
    ||  validNonAscii c

doubleQuoteLiteral :: Parser TextLiteral
doubleQuoteLiteral = do
    char '"'

    chunks <- many doubleQuoteChunk

    char '"'

    return (mconcat chunks)

singleQuoteContinue :: Parser TextLiteral
singleQuoteContinue =
        (interpolation <> singleQuoteContinue)
    <|> (escapedQuotePair <> singleQuoteContinue)
    <|> (escapedInterpolation <> singleQuoteContinue)
    <|> (do "''"; return mempty)
    <|> (singleQuoteChar <> singleQuoteContinue)

escapedQuotePair :: Parser TextLiteral
escapedQuotePair = do "'''"; return (Chunks [] "''")

escapedInterpolation :: Parser TextLiteral
escapedInterpolation = do
    "''${"

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
    "''"

    endOfLine

    singleQuoteContinue

interpolation :: Parser TextLiteral
interpolation = do
    "${"

    e <- completeExpression

    "}"

    return (Chunks [("", e)] "")

textLiteral :: Parser TextLiteral
textLiteral = doubleQuoteLiteral <|> singleQuoteLiteral

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

keyword :: Parser ()
keyword =
        if_
    <|> then_
    <|> else_
    <|> let_
    <|> in_
    <|> using
    <|> void missing
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

missing :: Parser ImportType
missing = do "missing"; return Missing

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
_NaturalFold = do "Natural/fold"; return NaturalFold

_NaturalBuild :: Parser Builtin
_NaturalBuild = do "Natural/build"; return NaturalBuild

_NaturalIsZero :: Parser Builtin
_NaturalIsZero = do "Natural/isZero"; return NaturalIsZero

_NaturalEven :: Parser Builtin
_NaturalEven = do "Natural/even"; return NaturalEven

_NaturalOdd :: Parser Builtin
_NaturalOdd = do "Natural/odd"; return NaturalOdd

_NaturalToInteger :: Parser Builtin
_NaturalToInteger = do "Natural/toInteger"; return NaturalToInteger

_NaturalShow :: Parser Builtin
_NaturalShow = do "Natural/show"; return NaturalShow

_IntegerToDouble :: Parser Builtin
_IntegerToDouble = do "Integer/toDouble"; return IntegerToDouble

_IntegerShow :: Parser Builtin
_IntegerShow = do "Integer/show"; return IntegerShow

_IntegerNegate :: Parser Builtin
_IntegerNegate = do "Integer/negate"; return IntegerNegate

_IntegerClamp :: Parser Builtin
_IntegerClamp = do "Integer/clamp"; return IntegerClamp

_NaturalSubtract :: Parser Builtin
_NaturalSubtract = do "Natural/subtract"; return NaturalSubtract

_DoubleShow :: Parser Builtin
_DoubleShow = do "Double/show"; return DoubleShow

_ListBuild :: Parser Builtin
_ListBuild = do "List/build"; return ListBuild

_ListFold :: Parser Builtin
_ListFold = do "List/fold"; return ListFold

_ListLength :: Parser Builtin
_ListLength = do "List/length"; return ListLength

_ListHead :: Parser Builtin
_ListHead = do "List/head"; return ListHead

_ListLast :: Parser Builtin
_ListLast = do "List/last"; return ListLast

_ListIndexed :: Parser Builtin
_ListIndexed = do "List/indexed"; return ListIndexed

_ListReverse :: Parser Builtin
_ListReverse = do "List/reverse"; return ListReverse

_TextShow :: Parser Builtin
_TextShow = do "Text/show"; return TextShow

_TextReplace :: Parser Builtin
_TextReplace = do "Text/replace"; return TextReplace

_Bool :: Parser Builtin
_Bool = do "Bool"; return Bool

_True :: Parser Builtin
_True = do "True"; return Syntax.True

_False :: Parser Builtin
_False = do "False"; return Syntax.False

_Optional :: Parser Builtin
_Optional = do "Optional"; return Optional

_None :: Parser Builtin
_None = do "None"; return None

_Natural :: Parser Builtin
_Natural = do "Natural"; return Natural

_Integer :: Parser Builtin
_Integer = do "Integer"; return Integer

_Double :: Parser Builtin
_Double = do "Double"; return Double

_Text :: Parser Builtin
_Text = do "Text"; return Text

_List :: Parser Builtin
_List = do "List"; return List

_Location :: Parser ()
_Location = void "Location"

constant :: Parser Constant
constant =
        _Type
    <|> _Kind
    <|> _Sort

_Type :: Parser Constant
_Type = do "Type"; return Type

_Kind :: Parser Constant
_Kind = do "Kind"; return Kind

_Sort :: Parser Constant
_Sort = do "Sort"; return Sort

combine :: Parser Operator
combine = do "∧" <|> "/\\"; return CombineRecordTerms

combineTypes :: Parser Operator
combineTypes = do "⩓" <|> "//\\\\"; return CombineRecordTypes

equivalent :: Parser Operator
equivalent = do "≡" <|> "==="; return Equivalent

prefer :: Parser Operator
prefer = do "⫽" <|> "//"; return Prefer

lambda :: Parser ()
lambda = do "λ" <|> "\\"; return ()

arrow :: Parser ()
arrow = do "→" <|> "->"; return ()

complete :: Parser ()
complete = do "::"; return ()

sign :: Num n => Parser (n -> n)
sign = (do "+"; return id) <|> (do "-"; return negate)

exponent :: Parser Int
exponent = do
    "e"

    s <- sign

    digits <- atLeast 1 (satisfy digit)

    return (s (digits `base` 10))

numericDoubleLiteral :: Parser Double
numericDoubleLiteral = do
    s <- sign

    digits0 <- atLeast 1 (satisfy digit)

    let withRadix = do
            "."

            digits1 <- atLeast 1 (satisfy digit)

            e <- exponent <|> pure 0

            return (s (fromInteger ((digits0 <> digits1) `base` 10) * 10^(e - length digits1)))

    let withoutRadix = do
            e <- exponent

            return (s (fromInteger (digits0 `base` 10) * 10^e))

    withRadix <|> withoutRadix

minusInfinityLiteral :: Parser Double
minusInfinityLiteral = do
    "-"

    _Infinity

    return (-1/0)

plusInfinityLiteral :: Parser Double
plusInfinityLiteral = do
    _Infinity

    return (1/0)

doubleLiteral :: Parser Double
doubleLiteral =
        numericDoubleLiteral
    <|> minusInfinityLiteral
    <|> plusInfinityLiteral
    <|> (do _NaN; return (0/0))

naturalLiteral :: Parser Natural
naturalLiteral = hexadecimal <|> decimal <|> zero
  where
    hexadecimal = do
        "0x"

        digits <- atLeast 1 (satisfy hexDig)

        return (digits `base` 16)

    decimal = do
        digit0 <- satisfy (between '1' '9')

        digits1 <- many (satisfy digit)

        return ((digit0 : digits1) `base` 10)

    zero = do
        "0"

        return 0

integerLiteral :: Parser Integer
integerLiteral = do
    s <- sign

    n <- naturalLiteral

    return (s (fromIntegral n))

identifier :: Parser Expression
identifier = fmap Constant constant <|> fmap Builtin builtin <|> variable

variable :: Parser Expression
variable = do
    x <- nonreservedLabel

    n <- index <|> pure 0

    return (Variable x n)
  where
    index = do
        try (do whsp; "@")

        whsp

        naturalLiteral

pathCharacter :: Char -> Bool
pathCharacter c =
        c == '\x21'
    ||  between '\x24' '\x27' c
    ||  between '\x2A' '\x2B' c
    ||  between '\x2D' '\x2E' c
    ||  between '\x30' '\x3B' c
    ||  c == '\x3D'
    ||  between '\x40' '\x5A' c
    ||  between '\x5E' '\x7A' c
    ||  c == '\x7C'
    ||  c == '\x7E'

quotedPathCharacter :: Char -> Bool
quotedPathCharacter c =
        between '\x20' '\x21' c
    ||  between '\x23' '\x2E' c
    ||  between '\x30' '\x7F' c
    ||  validNonAscii c

unquotedPathComponent :: Parser Text
unquotedPathComponent = takeWhile1 pathCharacter

quotedPathComponent :: Parser Text
quotedPathComponent = takeWhile1 quotedPathCharacter

pathComponent :: Parser Text
pathComponent = do
    "/"

    let quoted = do
            "\""

            component <- quotedPathComponent

            "\""

            return component

    unquotedPathComponent <|> quoted

path_ :: Parser File
path_ = do
    components <- Combinators.NonEmpty.some pathComponent

    return (File (NonEmpty.init components) (NonEmpty.last components))

local :: Parser ImportType
local = parentPath <|> herePath <|> homePath <|> absolutePath

parentPath :: Parser ImportType
parentPath = do
    ".."

    p <- path_

    return (Path Parent p)

herePath :: Parser ImportType
herePath = do
    "."

    p <- path_

    return (Path Here p)

homePath :: Parser ImportType
homePath = do
    "~"

    p <- path_

    return (Path Home p)

absolutePath :: Parser ImportType
absolutePath = do
    p <- path_

    return (Path Absolute p)

scheme_ :: Parser Scheme
scheme_ = do
    "http"

    let secure = do
            "s"

            return HTTPS

    secure <|> return HTTP

httpRaw :: Parser URL
httpRaw = do
    s <- scheme_

    "://"

    a <- authority_

    p <- pathAbempty

    q <- optional (do "?"; query_)

    return (URL s a p q)

pathAbempty :: Parser File
pathAbempty = do
    segments <- many (do "/"; segment)

    case segments of
        [] -> do
            return (File [] "")
        s : ss -> do
            let n = s :| ss

            return (File (NonEmpty.init n) (NonEmpty.last n))

authority_ :: Parser Text
authority_ = do
    (try (userinfo <> "@") <|> "") <> host <> ((":" <> port) <|> "")

userinfo :: Parser Text
userinfo = do
    let character = do
            c <- satisfy (\c -> unreserved c || subDelims c || c == ':')

            return (Text.singleton c)

    texts <- many (character <|> pctEncoded)

    return (Text.concat texts)

host :: Parser Text
host = ipLiteral <|> ipv4Address <|> domain

port :: Parser Text
port = takeWhile digit

ipLiteral :: Parser Text
ipLiteral = "[" <> (ipv6Address <|> ipvFuture) <> "]"

ipvFuture :: Parser Text
ipvFuture = do
        "v"
    <>  takeWhile1 hexDig
    <>  "."
    <>  takeWhile1 (\c -> unreserved c || subDelims c || c == ':')

ipv6Address :: Parser Text
ipv6Address =
        try option0
    <|> try option1
    <|> try option2
    <|> try option3
    <|> try option4
    <|> try option5
    <|> try option6
    <|> option7
  where
    option0 = do
        a <- replicateM 6 (h16 <> ":")

        b <- ls32

        return (Text.concat (a <> [ b ]))

    option1 = do
        a <- (h16 <|> "")

        b <- "::"

        c <- replicateM 4 (h16 <> ":")

        d <- ls32

        return (Text.concat ([ a, b ] <> c <> [ d ]))

    option2 = do
        let prefix = do
                a <- h16

                b <- atLeast 1 (":" <> h16)

                return (Text.concat (a : b))

        a <- prefix <|> ""

        b <- "::"

        c <- replicateM 3 (h16 <> ":")

        d <- ls32

        return (Text.concat ([ a, b ] <> c <> [ d ]))

    option3 = do
        let prefix = do
                a <- h16

                b <- atLeast 2 (":" <> h16)

                return (Text.concat (a : b))

        a <- prefix <|> ""

        b <- "::"

        c <- replicateM 2 (h16 <> ":")

        d <- ls32

        return (Text.concat ([ a, b ] <> c <> [ d ]))

    option4 = do
        let prefix = do
                a <- h16

                b <- atLeast 3 (":" <> h16)

                return (Text.concat (a : b))

        (prefix <|> "") <> "::" <> h16 <> ":" <> ls32

    option5 = do
        let prefix = do
                a <- h16

                b <- atLeast 4 (":" <> h16)

                return (Text.concat (a : b))

        (prefix <|> "") <> "::" <> ls32

    option6 = do
        let prefix = do
                a <- h16

                b <- atLeast 5 (":" <> h16)

                return (Text.concat (a : b))

        (prefix <|> "") <> "::" <> h16

    option7 = do
        let prefix = do
                a <- h16

                b <- atLeast 6 (":" <> h16)

                return (Text.concat (a : b))

        (prefix <|> "") <> "::"

h16 :: Parser Text
h16 = do
    a <- satisfy hexDig 

    b <- replicateM 3 (satisfy hexDig)

    return (Text.pack (a : b))

ls32 :: Parser Text
ls32 = (h16 <> ":" <> h16) <|> ipv4Address

ipv4Address :: Parser Text
ipv4Address = decOctet <> "." <> decOctet <> "." <> decOctet <> "." <> decOctet

decOctet :: Parser Text
decOctet = do
        try beginsWith25
    <|> try beginsWith2
    <|> try beginsWith1
    <|> try twoDigits
    <|> oneDigit
  where
    beginsWith25 = do
        a <- "25"

        b <- satisfy (between '\x30' '\x35')

        return (a <> Text.singleton b)

    beginsWith2 = do
        a <- "2"

        b <- satisfy (between '\x30' '\x34')

        c <- satisfy digit

        return (a <> Text.singleton b <> Text.singleton c)

    beginsWith1 = do
        a <- "1"

        b <- replicateM 2 (satisfy digit)

        return (a <> Text.pack b)

    twoDigits = do
        a <- satisfy (between '\x31' '\x39')

        b <- satisfy digit

        return (Text.pack [a, b])

    oneDigit = do
        b <- satisfy digit

        return (Text.singleton b)

domain :: Parser Text
domain = do
    a <- domainlabel

    b <- many ("." <> domainlabel)

    c <- "." <|> ""

    return (a <> Text.concat b <> c)

domainlabel :: Parser Text
domainlabel = do
    a <- takeWhile1 alphaNum

    b <- many (takeWhile1 ('-' ==) <> takeWhile1 alphaNum)

    return (a <> Text.concat b)

segment :: Parser Text
segment = do
    a <- many pchar

    return (Text.concat a)

pchar :: Parser Text
pchar = character <|> pctEncoded
  where
    character = do
        c <- satisfy (\c -> unreserved c || subDelims c || c `elem` [ ':', '@' ])

        return (Text.singleton c)

query_ :: Parser Text
query_ = do
    let character = do
            c <- satisfy (\c -> c `elem` [ '/', '?' ])

            return (Text.singleton c)

    a <- many (pchar <|> character)

    return (Text.concat a)

pctEncoded :: Parser Text
pctEncoded = do
    a <- "%"

    b <- satisfy hexDig

    c <- satisfy hexDig

    return (a <> Text.pack [ b, c ])

unreserved :: Char -> Bool
unreserved c = alphaNum c || c `elem` [ '-', '.', '_', '~' ]

subDelims :: Char -> Bool
subDelims c = c `elem` [ '!', '$', '&', '\'', '*', '+', ';', '=' ]

http :: Parser ImportType
http = do
    url <- httpRaw

    headers <- optional do
        try (do whsp; using)

        whsp

        importExpression

    return (Remote url headers)

env :: Parser ImportType
env = do
    "env:"

    let posix = do
            "\""

            v <- posixEnvironmentVariable

            "\""

            return v

    v <- bashEnvironmentVariable <|> posix

    return (Env v)

bashEnvironmentVariable :: Parser Text
bashEnvironmentVariable = do
    a <- satisfy (\c -> alpha c || c == '_')

    b <- takeWhile1 (\c -> alphaNum c || c == '_')

    return (Text.cons a b)

posixEnvironmentVariable :: Parser Text
posixEnvironmentVariable = do
    a <- some posixEnvironmentVariableCharacter

    return (Text.pack a)

posixEnvironmentVariableCharacter :: Parser Char
posixEnvironmentVariableCharacter = do
    let escaped = do
            "\\"

            let remainder =
                         (do "\""; return '"' )
                    <|>  (do "\\"; return '\\')
                    <|>  (do "a" ; return '\a')
                    <|>  (do "b" ; return '\b')
                    <|>  (do "f" ; return '\f')
                    <|>  (do "n" ; return '\n')
                    <|>  (do "r" ; return '\r')
                    <|>  (do "t" ; return '\t')
                    <|>  (do "v" ; return '\v')

            remainder

    let unescaped c =
                between '\x20' '\x21' c
            ||  between '\x23' '\x3C' c
            ||  between '\x3E' '\x5B' c
            ||  between '\x5D' '\x7E' c

    escaped <|> satisfy unescaped

importType :: Parser ImportType
importType =
    missing <|> local <|> http <|> env

hash :: Parser (Digest SHA256)
hash = do
    "sha256:"

    hexDigits <- replicateM 64 (satisfy hexDig)

    let base16 = Text.Encoding.encodeUtf8 (Text.pack hexDigits)

    bytes <-  case ByteArray.Encoding.convertFromBase Base16 base16 of
        Left string -> fail string
        Right bytes -> return (bytes :: ByteString)

    case Hash.digestFromByteString bytes of
        Nothing -> fail "Invalid sha256 hash"
        Just h  -> return h

import_ :: Parser Expression
import_ = do
    i <- importType

    h <- optional (try (do whsp1; hash))

    let location = do
            try (do whsp; as)

            whsp1

            (do _Text; return RawText) <|> (do _Location; return Location)

    l <- location <|> return Code

    return (Import i l h)

expression :: Parser Expression
expression =
        (do lambda

            whsp

            "("

            whsp

            x <- nonreservedLabel

            whsp

            ":"

            whsp1

            _A <- expression

            whsp

            ")"

            whsp

            arrow

            whsp

            _B <- expression

            return (Lambda x _A _B)
        )
    <|> (do if_

            whsp1

            a <- expression

            whsp

            then_

            whsp1

            b <- expression

            whsp

            else_

            whsp1

            c <- expression

            return (If a b c)
        )
    <|> (do bindings <- many letBinding

            in_

            whsp1

            b <- expression

            return (foldr (\(x, mA, a) -> Let x mA a) b bindings)
        )
    <|> (do forall

            whsp

            "("

            whsp

            x <- nonreservedLabel

            whsp

            ":"

            whsp1

            _A <- expression

            whsp

            ")"

            whsp

            arrow

            whsp

            _B <- expression

            return (Forall x _A _B)
        )
    <|> try (do
            _A <- operatorExpression

            whsp

            arrow

            whsp

            _B <- expression

            return (Forall "_" _A _B)
        )
    <|> try withExpression
    <|> try (do
            merge

            whsp1

            a <- importExpression

            whsp1

            b <- importExpression

            whsp

            ":"

            whsp1

            c <- applicationExpression

            return (Merge a b (Just c))
        )
    <|> try emptyListLiteral
    <|> (do toMap

            whsp1

            a <- importExpression

            whsp

            ":"

            whsp1

            b <- applicationExpression

            return (ToMap a (Just b))
        )
    <|> (do assert

            whsp

            ":"

            whsp1

            a <- expression

            return (Assert a)
        )
    <|> annotatedExpression

annotatedExpression :: Parser Expression
annotatedExpression = do
    a <- operatorExpression

    let annotation = do
            try (do whsp; ":")

            whsp1

            _A <- expression

            return (Annotation a _A)

    annotation <|> return a

letBinding :: Parser (Text, Maybe Expression, Expression)
letBinding = do
    let_

    whsp1

    x <- nonreservedLabel

    whsp

    mA <- optional do
        ":"

        whsp1

        _A <- expression

        whsp

        return _A

    "="

    whsp

    a <- expression

    whsp

    return (x, mA, a)

emptyListLiteral :: Parser Expression
emptyListLiteral = do
    "["

    whsp

    optional (do ","; whsp)

    "]"

    whsp

    ":"

    whsp1

    _A <- applicationExpression

    return (EmptyList _A)

withExpression :: Parser Expression
withExpression = do
    a <- importExpression

    clauses <- Combinators.NonEmpty.some do
        try (do whsp1; with)

        whsp1

        withClause

    return (foldl (\e (ks, v) -> With e ks v) a clauses)

withClause :: Parser (NonEmpty Text, Expression)
withClause = do
    k <- anyLabelOrSome

    ks <- many (do try (do whsp; "."); whsp; anyLabelOrSome)

    whsp

    "="

    whsp

    v <- operatorExpression

    return (k :| ks, v)

operatorExpression :: Parser Expression
operatorExpression = equivalentExpression

makeOperator :: Parser Operator -> Parser Expression -> Parser Expression
makeOperator parseOperator parseNext = do
    l0 <- parseNext

    fs <- many do
        operator <- try (do whsp; parseOperator)

        whsp

        r <- parseNext

        return (\l -> Operator l operator r)

    return (foldl (\l f -> f l) l0 fs)

makeOperator1 :: Parser Operator -> Parser Expression -> Parser Expression
makeOperator1 parseOperator parseNext = do
    l0 <- parseNext

    fs <- many do
        operator <- try (do whsp; parseOperator)

        whsp1

        r <- parseNext

        return (\l -> Operator l operator r)

    return (foldl (\l f -> f l) l0 fs)

equivalentExpression :: Parser Expression
equivalentExpression =
    makeOperator (do equivalent; return Equivalent) importAltExpression

importAltExpression :: Parser Expression
importAltExpression =
    makeOperator1 (do "?"; return Alternative) orExpression

orExpression :: Parser Expression
orExpression = makeOperator (do "||"; return Or) plusExpression

plusExpression :: Parser Expression
plusExpression = makeOperator1 (do "+"; return Plus) textAppendExpression

textAppendExpression :: Parser Expression
textAppendExpression =
    makeOperator (do "++"; return TextAppend) listAppendExpression

listAppendExpression :: Parser Expression
listAppendExpression =
    makeOperator (do "#"; return ListAppend) andExpression

andExpression :: Parser Expression
andExpression = makeOperator (do "&&"; return And) combineExpression

combineExpression :: Parser Expression
combineExpression =
    makeOperator (do combine; return CombineRecordTerms) preferExpression

preferExpression :: Parser Expression
preferExpression =
    makeOperator (do prefer; return Prefer) combineTypesExpression

combineTypesExpression :: Parser Expression
combineTypesExpression =
    makeOperator (do combineTypes; return CombineRecordTypes) timesExpression

timesExpression :: Parser Expression
timesExpression = makeOperator (do "*"; return Times) equalExpression

equalExpression :: Parser Expression
equalExpression = makeOperator (do "=="; return Equal) notEqualExpression

notEqualExpression :: Parser Expression
notEqualExpression =
    makeOperator (do "!="; return NotEqual) applicationExpression

applicationExpression :: Parser Expression
applicationExpression = do
    a <- firstApplicationExpression

    bs <- many (try (do whsp1; importExpression))

    return (foldl Application a bs)

firstApplicationExpression :: Parser Expression
firstApplicationExpression =
        (do merge

            whsp1

            a <- importExpression

            whsp1

            b <- importExpression

            return (Merge a b Nothing)
        )
    <|> (do _Some

            whsp1

            a <- importExpression

            return (Some a)
        )
    <|> (do toMap

            whsp1

            a <- importExpression

            return (ToMap a Nothing)
        )
    <|> importExpression

importExpression :: Parser Expression
importExpression = import_ <|> completionExpression

completionExpression :: Parser Expression
completionExpression = do
    a <- selectorExpression

    let selection = do
            try (do whsp; complete)

            whsp

            b <- selectorExpression

            return (Completion a b)

    selection <|> return a

selectorExpression :: Parser Expression
selectorExpression = do
    e0 <- primitiveExpression

    fs <- many (do try (do whsp; "."); whsp; selector)

    return (foldl (\e f -> f e) e0 fs)

selector :: Parser (Expression -> Expression)
selector =
        (do x  <- anyLabel    ; return (\e -> Field           e x ))
    <|> (do ks <- labels      ; return (\e -> ProjectByLabels e ks))
    <|> (do t  <- typeSelector; return (\e -> ProjectByType   e t ))

labels :: Parser [Text]
labels = do
    "{"

    whsp

    optional (do ","; whsp)

    let nonEmpty = do
            k0 <- anyLabelOrSome

            whsp

            ks <- many do

                ","

                whsp

                k <- anyLabelOrSome

                whsp

                return k

            optional (do ","; whsp)

            return (k0 : ks)

    ks <- nonEmpty <|> pure []

    "}"

    return ks

typeSelector :: Parser Expression
typeSelector = do
    "("

    whsp

    e <- expression

    whsp

    ")"

    return e

primitiveExpression :: Parser Expression
primitiveExpression =
        (do n <- try doubleLiteral; return (DoubleLiteral n))
    <|> (do n <- naturalLiteral; return (NaturalLiteral n))
    <|> (do n <- integerLiteral; return (IntegerLiteral n))
    <|> (do t <- textLiteral; return (TextLiteral t))
    <|> (do "{"

            whsp

            optional (do ","; whsp)

            e <- recordTypeOrLiteral

            whsp

            "}"

            return e
        )
    <|> (do "<"

            whsp

            optional (do "|"; whsp)

            e <- unionType

            whsp

            ">"

            return e
        )
    <|> nonEmptyListLiteral
    <|> identifier
    <|> (do "("; e <- completeExpression; ")"; return e)

recordTypeOrLiteral :: Parser Expression
recordTypeOrLiteral =
        emptyRecordLiteral
    <|> nonEmptyRecordTypeOrLiteral
    <|> return (RecordType [])

emptyRecordLiteral :: Parser Expression
emptyRecordLiteral =
    do "="; optional (do try (do whsp; ",")); return (RecordLiteral [])

nonEmptyRecordTypeOrLiteral :: Parser Expression
nonEmptyRecordTypeOrLiteral = try nonEmptyRecordType <|> nonEmptyRecordLiteral

nonEmptyRecordType :: Parser Expression
nonEmptyRecordType = do
    kt <- recordTypeEntry

    kts <- many (do try (do whsp; ","); whsp; recordTypeEntry)

    optional (try (do whsp; ","))

    return (RecordType (kt : kts))

recordTypeEntry :: Parser (Text, Expression)
recordTypeEntry = do
    k <- anyLabelOrSome

    whsp

    ":"

    whsp1

    t <- expression

    return (k, t)

nonEmptyRecordLiteral :: Parser Expression
nonEmptyRecordLiteral = do
    kv <- recordLiteralEntry

    kvs <- many (do try (do whsp; ","); whsp; recordLiteralEntry)

    optional (try (do whsp; ","))

    return (RecordLiteral (kv : kvs))

recordLiteralEntry :: Parser (Text, Expression)
recordLiteralEntry = do
    k0 <- anyLabelOrSome

    let normalEntry = do
            (ks, v) <- recordLiteralNormalEntry

            return (k0, foldr (\k e -> RecordLiteral [(k, e)]) v ks)

    let punnedEntry = do
            return (k0, Variable k0 0)

    normalEntry <|> punnedEntry

recordLiteralNormalEntry :: Parser ([Text], Expression)
recordLiteralNormalEntry = do
    ks <- many (do try (do whsp; "."); anyLabelOrSome)

    whsp

    "="

    whsp

    v <- expression

    return (ks, v)

unionType :: Parser Expression
unionType =
        (do kt <- unionTypeEntry

            kts <- many (do try (do whsp; "|"); whsp; unionTypeEntry)

            optional (try (do whsp; "|"))

            return (UnionType (kt : kts))
        )
    <|> return (UnionType [])

unionTypeEntry :: Parser (Text, Maybe Expression)
unionTypeEntry = do
    k <- anyLabelOrSome

    t <- optional (do try (do whsp; ":"); whsp1; expression)

    return (k, t)

nonEmptyListLiteral :: Parser Expression
nonEmptyListLiteral = do
    "["

    whsp

    optional (do ","; whsp)

    e0 <- expression

    whsp

    es <- many (do ","; whsp; e <- expression; whsp; return e)

    optional (do ","; whsp)

    "]"

    return (NonEmptyList (e0 :| es))

completeExpression :: Parser Expression
completeExpression = do whsp; e <- expression; whsp; return e
