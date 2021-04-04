{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{-| This module translates @./dhall.abnf@ into a parser implemented using an
    LL parser combinator package

    This parser optimizes for exactly corresponding to the ABNF grammar, at the
    expense of efficiency
-}
module Parser where

import Control.Applicative (Alternative(..), optional)
import Control.Monad (replicateM)
import Crypto.Hash (Digest, SHA256)
import Data.ByteArray.Encoding (Base(..))
import Data.ByteString (ByteString)
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Text.Earley (Grammar, Prod, namedToken, rule, satisfy)
import Prelude hiding (exponent, takeWhile)

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

import qualified Crypto.Hash                        as Hash
import qualified Data.ByteArray.Encoding            as ByteArray.Encoding
import qualified Data.Char                          as Char
import qualified Data.List.NonEmpty                 as NonEmpty
import qualified Data.Map                           as Map
import qualified Data.Text                          as Text
import qualified Data.Text.Encoding                 as Text.Encoding
import qualified Multiline

import qualified Control.Applicative.Combinators.NonEmpty as Combinators.NonEmpty

type Parser r = Prod r Char Char

between :: Char -> Char -> Char -> Bool
between lo hi c = lo <= c && c <= hi

char :: Char -> Parser r Char
char = namedToken

text :: Text -> Parser r Text
text t = fmap Text.pack (traverse char (Text.unpack t))

takeWhile :: (Char -> Bool) -> Parser r Text
takeWhile predicate = fmap Text.pack (many (satisfy predicate))

takeWhile1 :: (Char -> Bool) -> Parser r Text
takeWhile1 predicate = fmap Text.pack (some (satisfy predicate))

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

atMost :: Int -> Parser r a -> Parser r [a]
atMost 0 _ = do
    pure []
atMost n parser = (do
    x <- parser

    xs <- atMost (n - 1) parser

    return (x : xs) ) <|> pure []

atLeast :: Int -> Parser r a -> Parser r [a]
atLeast lowerBound parser = do
    prefix <- replicateM lowerBound parser

    suffix <- many parser

    return (prefix <> suffix)

range :: Int -> Int -> Parser r a -> Parser r [a]
range lowerBound upperBound parser = do
    prefix <- replicateM lowerBound parser

    suffix <- atMost (upperBound - lowerBound) parser

    return (prefix <> suffix)

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

grammar :: forall r . Grammar r (Parser r Expression)
grammar = mdo
    endOfLine <- rule (text "\n" <|> text "\r\n")

    let validNonAscii :: Char -> Bool
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

    let tab :: Char
        tab = '\t'

    blockComment <- rule do text "{-"; blockCommentContinue; return ()

    blockCommentChar <- rule
        (   void (satisfy (between '\x20' '\x7F'))
        <|> void (satisfy validNonAscii)
        <|> void (char tab)
        <|> void endOfLine
        )

    blockCommentContinue <- rule
        (   void (text "-}")
        <|> (do blockComment; blockCommentContinue; return ())
        <|> (do blockCommentChar; blockCommentContinue; return ())
        )

    notEndOfLine <- rule do
        let predicate :: Char -> Bool
            predicate c =
                    between '\x20' '\x7F' c
                ||  validNonAscii c
                ||  tab == c

        void (satisfy predicate)

    lineComment <- rule do
        text "--"
        many notEndOfLine
        void endOfLine
        return ()

    whitespaceChunk <- rule
        (   void (char ' ')
        <|> void (char tab)
        <|> void endOfLine
        <|> lineComment
        <|> blockComment
        )

    whsp <- rule do void (many whitespaceChunk)

    whsp1 <- rule do void (some whitespaceChunk)

    let alpha :: Char -> Bool
        alpha c = between '\x41' '\x5A' c || between '\x61' '\x7A' c

    let digit :: Char -> Bool
        digit = between '\x30' '\x39'

    let alphaNum :: Char -> Bool
        alphaNum c = alpha c || digit c

    let hexUpTo :: Char -> Char -> Bool
        hexUpTo upperBound c =
            digit c || between 'A' upperBound (Char.toUpper c)

    let hexDig :: Char -> Bool
        hexDig = hexUpTo 'F'

    let simpleLabelFirstChar :: Char -> Bool
        simpleLabelFirstChar c = alpha c || c == '_'

    let simpleLabelNextChar :: Char -> Bool
        simpleLabelNextChar c = alphaNum c || c `elem` [ '-', '/', '_' ]

    simpleLabel <- rule do
        first <- satisfy simpleLabelFirstChar

        rest  <- takeWhile simpleLabelNextChar

        -- let l = Text.cons first rest

        -- guard (l `notElem` reservedKeywords)

        return (Text.cons first rest)

    let quotedLabelChar c = between '\x20' '\x5F' c || between '\x61' '\x7E' c

    quotedLabel <- rule do takeWhile quotedLabelChar

    label <- rule
        (   (do char '`'; l <- quotedLabel; char '`'; return l)
        <|> simpleLabel
        )

    nonreservedLabel <- rule
        (   (do char '`'; l <- quotedLabel; char '`'; return l)
        <|> simpleLabel
        )

    anyLabel <- rule do label

    anyLabelOrSome <- rule (anyLabel <|> text "Some")

    doubleQuoteChunk <- rule
        (   interpolation
        <|> (do char '\x5C';

                c <- doubleQuoteEscaped;

                return (Chunks [] (Text.singleton c))
            )
        <|> (do c <- satisfy doubleQuoteChar

                return (Chunks [] (Text.singleton c))
            )
        )

    doubleQuoteEscaped <- rule
        (   (do char '"' ; return '"' )
        <|> (do char '$' ; return '$' )
        <|> (do char '\\'; return '\\')
        <|> (do char '/' ; return '/' )
        <|> (do char 'b' ; return '\b')
        <|> (do char 'f' ; return '\f')
        <|> (do char 'n' ; return '\n')
        <|> (do char 'r' ; return '\r')
        <|> (do char 't' ; return '\t')
        <|> (do char 'u' ; c <- unicodeEscape; return c)
        )

    unicodeEscape <- rule do
        number <-
                unbracedEscape
            <|> (do char '{'; c <- bracedEscape; char '}'; return c)

        return (Char.chr number)

    unicodeSuffix <- rule do
        let beginsWithoutF = do
                digit0 <- satisfy (hexUpTo 'E')

                digits1 <- replicateM 3 (satisfy hexDig)

                return ((digit0 : digits1) `base` 16)

        let beginsWithF = do
                digit0 <- satisfy (caseInsensitive 'F')

                digits1 <- replicateM 2 (satisfy hexDig)

                digit2 <- satisfy (hexUpTo 'D')

                return ((digit0 : digits1 <> [ digit2 ]) `base` 16)

        beginsWithoutF <|> beginsWithF

    unbracedEscape <- rule do
        let beginsUpToC = do
                digit0 <- satisfy (hexUpTo 'C')

                digits1 <- replicateM 3 (satisfy hexDig)

                return ((digit0 : digits1) `base` 16)

        let beginsWithD = do
                digit0 <- satisfy (caseInsensitive 'D')

                digit1 <- satisfy (between '0' '7')

                digits2 <- replicateM 2 (satisfy hexDig)

                return ((digit0 : digit1 : digits2) `base` 16)

        let beginsWithE = do
                digit0 <- satisfy (caseInsensitive 'E')

                digits1 <- replicateM 3 (satisfy hexDig)

                return ((digit0 : digits1) `base` 16)

        let beginsWithF = do
                digit0 <- satisfy (caseInsensitive 'F')

                digits1 <- replicateM 2 (satisfy hexDig)

                digit2 <- satisfy (hexUpTo 'D')

                return ((digit0 : digits1 <> [ digit2 ]) `base` 16)

        beginsUpToC <|> beginsWithD <|> beginsWithE <|> beginsWithF

    bracedCodepoint <- rule do
        let planes1Through16 = do
                prefix <-
                        fmap digitToNumber (satisfy hexDig)
                    <|> (do text "10"; return 16)

                suffix <- unicodeSuffix

                return (prefix * 0x10000 + suffix)

        let threeDigits = do
                digits <- range 1 3 (satisfy hexDig)

                return (digits `base` 16)

        planes1Through16 <|> unbracedEscape <|> threeDigits

    let doubleQuoteChar :: Char -> Bool
        doubleQuoteChar c = do
                between '\x20' '\x21' c
            ||  between '\x23' '\x5B' c
            ||  between '\x5D' '\x7F' c
            ||  validNonAscii c

    bracedEscape <- rule do takeWhile (== '0'); c <- bracedCodepoint; return c

    doubleQuoteLiteral <- rule do
        char '"'

        chunks <- many doubleQuoteChunk

        char '"'

        return (mconcat chunks)

    singleQuoteContinue <- rule
        (   (interpolation <> singleQuoteContinue)
        <|> (escapedQuotePair <> singleQuoteContinue)
        <|> (escapedInterpolation <> singleQuoteContinue)
        <|> (do text "''"; return mempty)
        <|> (singleQuoteChar <> singleQuoteContinue)
        )

    escapedQuotePair <- rule do
        text "'''"

        return (Chunks [] "''")

    escapedInterpolation <- rule do
        text "''${"

        return (Chunks [] "${")

    singleQuoteChar <- rule do
        let predicate c = between '\x20' '\x7F' c || validNonAscii c || tab == c

        let nonNewline = do
                c <- satisfy predicate

                return (Chunks [] (Text.singleton c))

        let newLine = do
                t <- endOfLine

                return (Chunks [] t)

        nonNewline <|> newLine

    singleQuoteLiteral <- rule do
        text "''"

        endOfLine

        contents <- singleQuoteContinue

        return (Multiline.toDoubleQuotes contents)

    interpolation <- rule do
        text "${"

        e <- completeExpression

        char '}'

        return (Chunks [("", e)] "")

    textLiteral <- rule (doubleQuoteLiteral <|> singleQuoteLiteral)

    if_ <- rule do void (text "if")

    then_ <- rule do void (text "then")

    else_ <- rule do void (text "else")

    let_ <- rule do void (text "let")

    in_ <- rule do void (text "in")

    as <- rule do void (text "as")

    using <- rule do void (text "using")

    merge <- rule do void (text "merge")

    missing <- rule do text "missing"; return Missing

    _Infinity <- rule do void (text "Infinity")

    _NaN <- rule do void (text "NaN")

    _Some <- rule do void (text "Some")

    toMap <- rule do void (text "toMap")

    assert <- rule do void (text "assert")

    forallKeyword <- rule do void (text "forall")

    forallSymbol <- rule do void (char '∀')

    forall <- rule (forallSymbol <|> forallKeyword)

    with <- rule do
        void (text "with")

    builtin <- rule
        (   _NaturalFold
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
        )

    _NaturalFold <- rule do text "Natural/fold"; return NaturalFold

    _NaturalBuild <- rule do text "Natural/build"; return NaturalBuild

    _NaturalIsZero <- rule do text "Natural/isZero"; return NaturalIsZero

    _NaturalEven <- rule do text "Natural/even"; return NaturalEven

    _NaturalOdd <- rule do text "Natural/odd"; return NaturalOdd

    _NaturalToInteger <- rule do text "Natural/toInteger"; return NaturalToInteger

    _NaturalShow <- rule do text "Natural/show"; return NaturalShow

    _IntegerToDouble <- rule do text "Integer/toDouble"; return IntegerToDouble

    _IntegerShow <- rule do text "Integer/show"; return IntegerShow

    _IntegerNegate <- rule do text "Integer/negate"; return IntegerNegate

    _IntegerClamp <- rule do text "Integer/clamp"; return IntegerClamp

    _NaturalSubtract <- rule do text "Natural/subtract"; return NaturalSubtract

    _DoubleShow <- rule do text "Double/show"; return DoubleShow

    _ListBuild <- rule do text "List/build"; return ListBuild

    _ListFold <- rule do text "List/fold"; return ListFold

    _ListLength <- rule do text "List/length"; return ListLength

    _ListHead <- rule do text "List/head"; return ListHead

    _ListLast <- rule do text "List/last"; return ListLast

    _ListIndexed <- rule do text "List/indexed"; return ListIndexed

    _ListReverse <- rule do text "List/reverse"; return ListReverse

    _TextShow <- rule do text "Text/show"; return TextShow

    _TextReplace <- rule do text "Text/replace"; return TextReplace

    _Bool <- rule do text "Bool"; return Bool

    _True <- rule do text "True"; return Syntax.True

    _False <- rule do text "False"; return Syntax.False

    _Optional <- rule do text "Optional"; return Optional

    _None <- rule do text "None"; return None

    _Natural <- rule do text "Natural"; return Natural

    _Integer <- rule do text "Integer"; return Integer

    _Double <- rule do text "Double"; return Double

    _Text <- rule do text "Text"; return Text

    _List <- rule do text "List"; return List

    _Location <- rule do void (text "Location")

    constant <- rule (_Type <|> _Kind <|> _Sort)

    _Type <- rule do text "Type"; return Type

    _Kind <- rule do text "Kind"; return Kind

    _Sort <- rule do text "Sort"; return Sort

    combine <- rule do text "∧" <|> text "/\\"; return CombineRecordTerms

    combineTypes <- rule do text "⩓" <|> text "//\\\\"; return CombineRecordTypes

    equivalent <- rule do text "≡" <|> text "==="; return Equivalent

    prefer <- rule do text "⫽" <|> text "//"; return Prefer

    lambda <- rule do text "λ" <|> text "\\"; return ()

    arrow <- rule do text "→" <|> text "->"; return ()

    complete <- rule do text "::"; return ()

    let sign :: Num n => Parser r (n -> n)
        sign =  (do char '+'; return id)
            <|> (do char '-'; return negate)

    exponent <- rule do
        char 'e'

        s <- sign <|> pure id

        digits <- atLeast 1 (satisfy digit)

        return (s (digits `base` 10))

    numericDoubleLiteral <- rule do
        let withRadix = do
                char '.'

                digits1 <- atLeast 1 (satisfy digit)

                e <- exponent <|> pure 0

                return (\s digits0 -> s (fromInteger ((digits0 <> digits1) `base` 10) * 10^^(e - length digits1)))

        let withoutRadix = do
                e <- exponent

                return (\s digits0 -> s (fromInteger (digits0 `base` 10) * 10^^e))

        s <- sign <|> pure id

        digits0 <- atLeast 1 (satisfy digit)

        f <- withRadix <|> withoutRadix

        return (f s digits0)

    minusInfinityLiteral <- rule do char '-'; _Infinity; return (-1/0)

    plusInfinityLiteral <- rule do _Infinity; return (1/0)

    doubleLiteral <- rule
        (   minusInfinityLiteral
        <|> plusInfinityLiteral
        <|> (do _NaN; return (0/0))
        <|> numericDoubleLiteral
        )

    naturalLiteral <- rule do
        let hexadecimal = do
                text "0x"

                digits <- atLeast 1 (satisfy hexDig)

                return (digits `base` 16)

        let decimal = do
                digit0 <- satisfy (between '1' '9')

                digits1 <- many (satisfy digit)

                return ((digit0 : digits1) `base` 10)

        let zero = do char '0'; return 0

        hexadecimal <|> decimal <|> zero

    integerLiteral <- rule do
        s <- sign

        n <- naturalLiteral

        return (s (fromIntegral n))

    identifier <- rule
        (   fmap Constant constant
        <|> fmap Builtin builtin
        <|> variable
        )

    variable <- rule do
        let index = do whsp; char '@'; whsp; n <- naturalLiteral; return n

        x <- nonreservedLabel

        n <- index <|> pure 0

        return (Variable x n)

    let pathCharacter :: Char -> Bool
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

    let quotedPathCharacter :: Char -> Bool
        quotedPathCharacter c =
                between '\x20' '\x21' c
            ||  between '\x23' '\x2E' c
            ||  between '\x30' '\x7F' c
            ||  validNonAscii c

    unquotedPathComponent <- rule do takeWhile1 pathCharacter

    quotedPathComponent <- rule do takeWhile1 quotedPathCharacter

    pathComponent <- rule do
        char '/'

        let quoted = do
                char '"'

                component <- quotedPathComponent

                char '"'

                return component

        component <- unquotedPathComponent <|> quoted

        return component

    path_ <- rule do
        components <- Combinators.NonEmpty.some pathComponent

        return (File (reverse (NonEmpty.init components)) (NonEmpty.last components))

    local <- rule (parentPath <|> herePath <|> homePath <|> absolutePath)

    parentPath <- rule do
        text ".."

        p <- path_

        return (Path Parent p)

    herePath <- rule do
        char '.'

        p <- path_

        return (Path Here p)

    homePath <- rule do
        char '~'

        p <- path_

        return (Path Home p)

    absolutePath <- rule do
        p <- path_

        return (Path Absolute p)

    scheme_ <- rule do
        text "http"

        let secure = do
                char 's'

                return HTTPS

        s <- secure <|> pure HTTP

        return s

    httpRaw <- rule do
        s <- scheme_

        text "://"

        a <- authority_

        p <- pathAbempty

        q <- optional (do char '?'; q <- query_; return q)

        return (URL s a p q)

    pathAbempty <- rule do
        let adapt [] =
                File [] ""
            adapt (s : ss) =
                let n = s :| ss

                in  File (reverse (NonEmpty.init n)) (NonEmpty.last n)

        fmap adapt (many (do char '/'; s <- segment; return s))

    authority_ <- rule
        (  ((userinfo <> text "@") <|> pure "")
        <> host
        <> ((text ":" <> port) <|> pure "")
        )

    userinfo <- rule do
        let character = do
                c <- satisfy (\c -> unreserved c || subDelims c || c == ':')

                return (Text.singleton c)

        texts <- many (character <|> pctEncoded)

        return (Text.concat texts)

    host <- rule (ipLiteral <|> ipv4Address <|> domain)

    port <- rule do takeWhile digit

    ipLiteral <- rule (text "[" <> (ipv6Address <|> ipvFuture) <> text "]")

    ipvFuture <- rule
        (   text "v"
        <>  takeWhile1 hexDig
        <>  text "."
        <>  takeWhile1 (\c -> unreserved c || subDelims c || c == ':')
        )

    ipv6Address <- rule do
        let option0 = do
                a <- replicateM 6 (h16 <> text ":")

                b <- ls32

                return (Text.concat (a <> [ b ]))

        let option1 = do
                a <- text "::"

                b <- replicateM 5 (h16 <> text ":")

                c <- ls32

                return (Text.concat ([ a ] <> b <> [ c ]))

        let option2 = do
                a <- (h16 <|> pure "")

                b <- text "::"

                c <- replicateM 4 (h16 <> text ":")

                d <- ls32

                return (Text.concat ([ a, b ] <> c <> [ d ]))

        let option3 = do
                let prefix = do
                        a <- h16

                        b <- atLeast 1 (text ":" <> h16)

                        return (Text.concat (a : b))

                a <- prefix <|> pure ""

                b <- text "::"

                c <- replicateM 3 (h16 <> text ":")

                d <- ls32

                return (Text.concat ([ a, b ] <> c <> [ d ]))

        let option4 = do
                let prefix = do
                        a <- h16

                        b <- atLeast 2 (text ":" <> h16)

                        return (Text.concat (a : b))

                a <- prefix <|> pure ""

                b <- text "::"

                c <- replicateM 2 (h16 <> text ":")

                d <- ls32

                return (Text.concat ([ a, b ] <> c <> [ d ]))

        let option5 = do
                let prefix = do
                        a <- h16

                        b <- atLeast 3 (text ":" <> h16)

                        return (Text.concat (a : b))

                (prefix <|> pure "") <> text "::" <> h16 <> text ":" <> ls32

        let option6 = do
                let prefix = do
                        a <- h16

                        b <- atLeast 4 (text ":" <> h16)

                        return (Text.concat (a : b))

                (prefix <|> pure "") <> text "::" <> ls32

        let option7 = do
                let prefix = do
                        a <- h16

                        b <- atLeast 5 (text ":" <> h16)

                        return (Text.concat (a : b))

                (prefix <|> pure "") <> text "::" <> h16

        let option8 = do
                let prefix = do
                        a <- h16

                        b <- atLeast 6 (text ":" <> h16)

                        return (Text.concat (a : b))

                (prefix <|> pure "") <> text "::"

        (       option0
            <|> option1
            <|> option2
            <|> option3
            <|> option4
            <|> option5
            <|> option6
            <|> option7
            <|> option8
            )

    h16 <- rule do
        a <- satisfy hexDig

        b <- atMost 3 (satisfy hexDig)

        return (Text.pack (a : b))

    ls32 <- rule
        (   (h16 <> text ":" <> h16)
        <|> ipv4Address
        )

    ipv4Address <- rule
        (   decOctet
        <>  text "."
        <>  decOctet
        <>  text "."
        <>  decOctet
        <>  text "."
        <>  decOctet
        )

    decOctet <- rule do
        let beginsWith25 = do
                a <- text "25"

                b <- satisfy (between '\x30' '\x35')

                return (a <> Text.singleton b)

        let beginsWith2 = do
                a <- text "2"

                b <- satisfy (between '\x30' '\x34')

                c <- satisfy digit

                return (a <> Text.singleton b <> Text.singleton c)

        let beginsWith1 = do
                a <- text "1"

                b <- replicateM 2 (satisfy digit)

                return (a <> Text.pack b)

        let twoDigits = do
                a <- satisfy (between '\x31' '\x39')

                b <- satisfy digit

                return (Text.pack [a, b])

        let oneDigit = do
                b <- satisfy digit

                return (Text.singleton b)

        beginsWith25 <|> beginsWith2 <|> beginsWith1 <|> twoDigits <|> oneDigit

    domain <- rule do
        a <- domainlabel

        b <- many (text "." <> domainlabel)

        c <- text "." <|> pure ""

        return (a <> Text.concat b <> c)

    domainlabel <- rule do
        a <- takeWhile1 alphaNum

        b <- many (takeWhile1 ('-' ==) <> takeWhile1 alphaNum)

        return (a <> Text.concat b)

    segment <- rule do
        a <- many pchar

        return (Text.concat a)

    pchar <- rule do
        let character = do
                c <- satisfy (\c -> unreserved c || subDelims c || c `elem` [ ':', '@' ])

                return (Text.singleton c)

        character <|> pctEncoded

    query_ <- rule do
        let character = do
                c <- satisfy (\c -> c `elem` [ '/', '?' ])

                return (Text.singleton c)

        a <- many (pchar <|> character)

        return (Text.concat a)

    pctEncoded <- rule do
        a <- text "%"

        b <- satisfy hexDig

        c <- satisfy hexDig

        return (a <> Text.pack [ b, c ])

    let unreserved :: Char -> Bool
        unreserved c = alphaNum c || c `elem` [ '-', '.', '_', '~' ]

    let subDelims :: Char -> Bool
        subDelims c = c `elem` [ '!', '$', '&', '\'', '*', '+', ';', '=' ]

    http <- rule do
        url <- httpRaw

        headers <- optional do
            whsp

            using

            whsp

            i <- importExpression

            return i

        return (Remote url headers)

    env <- rule do
        text "env:"

        let posix = do
                char '"'

                v <- posixEnvironmentVariable

                char '"'

                return v

        v <- bashEnvironmentVariable <|> posix

        return (Env v)

    bashEnvironmentVariable <- rule do
        a <- satisfy (\c -> alpha c || c == '_')

        b <- takeWhile1 (\c -> alphaNum c || c == '_')

        return (Text.cons a b)

    posixEnvironmentVariable <- rule do
        a <- some posixEnvironmentVariableCharacter

        return (Text.pack a)

    posixEnvironmentVariableCharacter <- rule do
        let escaped = do
                char '\\'

                let remainder =
                             (do char '"' ; return '"' )
                        <|>  (do char '\\'; return '\\')
                        <|>  (do char 'a' ; return '\a')
                        <|>  (do char 'b' ; return '\b')
                        <|>  (do char 'f' ; return '\f')
                        <|>  (do char 'n' ; return '\n')
                        <|>  (do char 'r' ; return '\r')
                        <|>  (do char 't' ; return '\t')
                        <|>  (do char 'v' ; return '\v')

                c <- remainder

                return c

        let unescaped c =
                    between '\x20' '\x21' c
                ||  between '\x23' '\x3C' c
                ||  between '\x3E' '\x5B' c
                ||  between '\x5D' '\x7E' c

        escaped <|> satisfy unescaped

    importType :: Parser r ImportType <- rule
        (   missing
        <|> local
        <|> http
        <|> env
        )

    hash :: Parser r (Digest SHA256) <- rule do
        text "sha256:"

        let convert hexDigits =
                case Hash.digestFromByteString bytes of
                    Nothing -> error "Invalid sha256 hash"
                    Just d  -> d
              where
                base16 = Text.Encoding.encodeUtf8 (Text.pack hexDigits)

                bytes = case ByteArray.Encoding.convertFromBase Base16 base16 of
                    Left string -> error string
                    Right b     -> (b :: ByteString)

        hexDigits <- replicateM 64 (satisfy hexDig)

        return (convert hexDigits)

    import_ :: Parser r Expression <- rule do
        i <- importType

        h <- optional (do whsp1; h <- hash; return h)

        let mode = do
                whsp

                as

                whsp1

                m <- (do _Text; return RawText) <|> (do _Location; return Location)
                return m

        l <- mode <|> pure Code

        return (Import i l h)

    expression :: Parser r Expression <- rule
        (   (do lambda

                whsp

                char '('

                whsp

                x <- nonreservedLabel

                whsp

                char ':'

                whsp1

                _A <- expression

                whsp

                char ')'

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

                char '('

                whsp

                x <- nonreservedLabel

                whsp

                char ':'

                whsp1

                _A <- expression

                whsp

                char ')'

                whsp

                arrow

                whsp

                _B <- expression

                return (Forall x _A _B)
            )
        <|> (do _A <- operatorExpression

                whsp

                arrow

                whsp

                _B <- expression

                return (Forall "_" _A _B)
            )
        <|> withExpression
        <|> (do merge

                whsp1

                a <- importExpression

                whsp1

                b <- importExpression

                whsp

                char ':'

                whsp1

                c <- applicationExpression

                return (Merge a b (Just c))
            )
        <|> emptyListLiteral
        <|> (do toMap

                whsp1

                a <- importExpression

                whsp

                char ':'

                whsp1

                b <- applicationExpression

                return (ToMap a (Just b))
            )
        <|> (do assert

                whsp

                char ':'

                whsp1

                a <- expression

                return (Assert a)
            )
        <|> annotatedExpression
        )

    annotatedExpression :: Parser r Expression <- rule do
        let annotation = do
                whsp

                char ':'

                whsp1

                _A <- expression

                return (\a -> Annotation a _A)

        a <- operatorExpression

        f <- annotation <|> pure id

        return (f a)

    letBinding :: Parser r (Text, Maybe Expression, Expression) <- rule do
        let_

        whsp1

        x <- nonreservedLabel

        whsp

        mA <- optional do
            char ':'

            whsp1

            _A <- expression

            whsp

            return _A

        char '='

        whsp

        a <- expression

        whsp

        return (x, mA, a)

    emptyListLiteral :: Parser r Expression <- rule do
        char '['

        whsp

        optional (do char ','; whsp; return ())

        char ']'

        whsp

        char ':'

        whsp1

        _A <- applicationExpression

        return (EmptyList _A)

    withExpression :: Parser r Expression <- rule do
        a <- importExpression

        clauses <- Combinators.NonEmpty.some do
            whsp1

            with

            whsp1

            e <- withClause

            return e

        return (foldl (\e (ks, v) -> With e ks v) a clauses)

    withClause :: Parser r (NonEmpty Text, Expression) <- rule do
        k0 <- anyLabelOrSome

        ks <- many (do whsp; char '.'; whsp; k <- anyLabelOrSome; return k)

        whsp

        char '='

        whsp

        v <- operatorExpression

        return (k0 :| ks, v)

    operatorExpression :: Parser r Expression <- rule equivalentExpression

    let makeOperator
            :: Parser r Operator -> Parser r Expression -> Parser r Expression
        makeOperator parseOperator parseNext = do
            l0 <- parseNext

            fs <- many do
                whsp

                operator <- parseOperator

                whsp

                r <- parseNext

                return (\l -> Operator l operator r)

            return (foldl (\l f -> f l) l0 fs)

    let makeOperator1
            :: Parser r Operator -> Parser r Expression -> Parser r Expression
        makeOperator1 parseOperator parseNext = do
            l0 <- parseNext

            fs <- many do
                whsp

                operator <- parseOperator

                whsp1

                r <- parseNext

                return (\l -> Operator l operator r)

            return (foldl (\l f -> f l) l0 fs)

    equivalentExpression :: Parser r Expression <- rule do
        makeOperator (do equivalent; return Equivalent) importAltExpression

    importAltExpression :: Parser r Expression <- rule do
        makeOperator1 (do char '?'; return Alternative) orExpression

    orExpression :: Parser r Expression <- rule do
        makeOperator (do text "||"; return Or) plusExpression

    plusExpression :: Parser r Expression <- rule do
        makeOperator1 (do char '+'; return Plus) textAppendExpression

    textAppendExpression :: Parser r Expression <- rule do
        makeOperator (do text "++"; return TextAppend) listAppendExpression

    listAppendExpression :: Parser r Expression <- rule do
        makeOperator (do char '#'; return ListAppend) andExpression

    andExpression :: Parser r Expression <- rule do
        makeOperator (do text "&&"; return And) combineExpression

    combineExpression :: Parser r Expression <- rule do
        makeOperator (do combine; return CombineRecordTerms) preferExpression

    preferExpression :: Parser r Expression <- rule do
       makeOperator (do prefer; return Prefer) combineTypesExpression

    combineTypesExpression :: Parser r Expression <- rule do
        makeOperator (do combineTypes; return CombineRecordTypes) timesExpression

    timesExpression :: Parser r Expression <- rule do
        makeOperator (do char '*'; return Times) equalExpression

    equalExpression :: Parser r Expression <- rule do
        makeOperator (do text "=="; return Equal) notEqualExpression

    notEqualExpression :: Parser r Expression <- rule do
        makeOperator (do text "!="; return NotEqual) applicationExpression

    applicationExpression :: Parser r Expression <- rule do
        a <- firstApplicationExpression

        bs <- many (do whsp1; b <- importExpression; return b)

        return (foldl Application a bs)

    firstApplicationExpression :: Parser r Expression <- rule 
        (   (do merge

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
        )

    importExpression :: Parser r Expression <- rule
        (   import_
        <|> completionExpression
        )

    completionExpression :: Parser r Expression <- rule do
        let selection = do
                whsp

                complete

                whsp

                b <- selectorExpression

                return (\a -> Completion a b)

        a <- selectorExpression

        f <- selection <|> pure id

        return (f a)

    selectorExpression :: Parser r Expression <- rule do
        e0 <- primitiveExpression

        fs <- many (do whsp; char '.'; whsp; f <- selector; return f)

        return (foldl (\e f -> f e) e0 fs)

    selector :: Parser r (Expression -> Expression) <- rule
       (   (do x  <- anyLabel    ; return (\e -> Field           e x ))
       <|> (do ks <- labels      ; return (\e -> ProjectByLabels e ks))
       <|> (do t  <- typeSelector; return (\e -> ProjectByType   e t ))
       )

    labels :: Parser r [Text] <- rule do
        char '{'

        whsp

        optional (do char ','; whsp; return ())

        let nonEmpty = do
                k0 <- anyLabelOrSome

                whsp

                ks <- many do
                    char ','

                    whsp

                    k <- anyLabelOrSome

                    whsp

                    return k

                optional (do char ','; whsp; return ())

                return (k0 : ks)

        ks <- nonEmpty <|> pure []

        char '}'

        return ks

    typeSelector :: Parser r Expression <- rule do
        char '('

        whsp

        e <- expression

        whsp

        char ')'

        return e

    primitiveExpression :: Parser r Expression <- rule
        (   (do n <- doubleLiteral; return (DoubleLiteral n))
        <|> (do n <- naturalLiteral; return (NaturalLiteral n))
        <|> (do n <- integerLiteral; return (IntegerLiteral n))
        <|> (do t <- textLiteral; return (TextLiteral t))
        <|> (do char '{'

                whsp

                optional (do char ','; whsp; return ())

                e <- recordTypeOrLiteral

                whsp

                char '}'

                return e
            )
        <|> (do char '<'

                whsp

                optional (do char '|'; whsp; return ())

                e <- unionType

                whsp

                char '>'

                return e
            )
        <|> nonEmptyListLiteral
        <|> identifier
        <|> (do char '('; e <- completeExpression; char ')'; return e)
        )

    recordTypeOrLiteral :: Parser r Expression <- rule
        (   emptyRecordLiteral
        <|> nonEmptyRecordTypeOrLiteral
        <|> pure (RecordType [])
        )

    emptyRecordLiteral :: Parser r Expression <- rule do
        char '='

        optional (do whsp; char ','; return ())

        return (RecordLiteral [])

    nonEmptyRecordTypeOrLiteral :: Parser r Expression <- rule
        (   nonEmptyRecordType
        <|> nonEmptyRecordLiteral
        )

    nonEmptyRecordType :: Parser r Expression <- rule do
        kt0 <- recordTypeEntry

        kts <- many (do whsp; char ','; whsp; kt <- recordTypeEntry; return kt)

        optional (do whsp; char ','; return ())

        return (RecordType (kt0 : kts))

    recordTypeEntry :: Parser r (Text, Expression) <- rule do
        k <- anyLabelOrSome

        whsp

        char ':'

        whsp1

        t <- expression

        return (k, t)

    nonEmptyRecordLiteral :: Parser r Expression <- rule do
        let f kv0 kvs =
                Map.toList (Map.fromListWith combineRecordTerms (kv0 : kvs))
              where
                combineRecordTerms r l = Operator l CombineRecordTerms r

        kv0 <- recordLiteralEntry

        kvs <- many do
            whsp

            char ','

            whsp

            kv <- recordLiteralEntry

            return kv

        optional (do whsp; char ','; return ())

        return (RecordLiteral (f kv0 kvs))

    recordLiteralEntry :: Parser r (Text, Expression) <- rule do
        let normalEntry = do
                let f (ks, v) k0 =
                        (k0, foldr (\k e -> RecordLiteral [(k, e)]) v ks)

                ksv  <- recordLiteralNormalEntry

                return (f ksv)

        let punnedEntry = do
                pure (\k0 -> (k0, Variable k0 0))

        k0 <- anyLabelOrSome

        f <- normalEntry <|> punnedEntry

        return (f k0)

    recordLiteralNormalEntry :: Parser r ([Text], Expression) <- rule do
        ks <- do
            ks <- many (do whsp; char '.'; k <- anyLabelOrSome; return k)

            whsp

            char '='

            return ks

        whsp

        v <- expression

        return (ks, v)

    unionType :: Parser r Expression <- rule
        (   (do kt0 <- unionTypeEntry

                kts <- many do
                    whsp

                    char '|'

                    whsp

                    kt <- unionTypeEntry

                    return kt

                optional (do whsp; char '|'; return ())

                return (UnionType (kt0 : kts))
            )
        <|> pure (UnionType [])
        )

    unionTypeEntry :: Parser r (Text, Maybe Expression) <- rule do
        k <- anyLabelOrSome

        t <- optional (do whsp; char ':'; whsp1; t <- expression; return t)

        return (k, t)

    nonEmptyListLiteral :: Parser r Expression <- rule do
        char '['

        whsp

        optional (do char ','; whsp; return ())

        e0 <- expression

        whsp

        es <- many (do char ','; whsp; e <- expression; whsp; return e)

        optional (do char ','; whsp; return ())

        char ']'

        return (NonEmptyList (e0 :| es))

    completeExpression :: Parser r Expression <- rule do
        whsp

        e <- expression

        whsp

        return e

    return completeExpression
