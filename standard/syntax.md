The following notation is a simplified version of the syntax found in
`./dhall.abnf`.  This simplified notation is used for all of the following
judgments:

```
m, n = 0 / 1 + n  ; Natural numbers

d = ±n            ; Integers

x, y              ; Variables

; Mnemonics for the most commonly used labels:
;
; Terms are lowercase:
;
;     a    = input term whose type is "A"
;     b    = output term whose type is "B"
;     f    = "f"unction
;     l, r = "l"eft and "r"ight term that share the same type
;     e    = term whose type is "E"
;     t    = term whose type is "T"
;     u    = term whose type is "U"
;
; Types are uppercase:
;
;     A  = type of the input term "a"
;     B  = type of the output term "b"
;     E  = type of the term "e"
;     T  = type of the term "t"
;     U  = type of the term "u"
;
; Constants that are `Type`, `Kind`, or `Sort` are lowercase:
;
;     c = "c"onstant
;     i = function's "i"nput type
;     o = function's "o"utput type
;
; Similar terms are distinguished by subscripts like `a₀`, `a₁`, …
;
; A term that represents zero or more values or key-value pairs ends with `s…`,
; such as `as…`
;
; Note that these are only informal mnemonics.  Dhall is a pure type system,
; which means that many places in the syntax permit terms, types, kinds, and
; sorts. The typing judgments are the authoritative rules for what expressions
; are permitted and forbidden.
a, b, f, l, r, e, t, u, A, B, E, T, U, c, i, o
  = x@n                               ; Identifier (`x` is short-hand for `x@0`).
                                      ; Slash-names such as `Natural/isZero`
                                      ; are identifiers, not extra forms.
  / λ(x : A) → b                      ; Anonymous function
  / ∀(x : A) → B                      ; Function type
                                      ; (`A → B` is short-hand for `∀(_ : A) → B`)
  / let x : A = a in b                ; Let expression with type annotation
  / let x     = a in b                ; Let expression without type annotation
  / if t then l else r                ; if-then-else expression
  / merge t u : T                     ; Union elimination with type annotation
  / merge t u                         ; Union elimination
  / toMap t : T                       ; Conversion to a map with type annotation
  / toMap t                           ; Conversion to a map
  / [] : T                            ; Empty list literals with type annotation
  / [ t, ts… ]                        ; Non-empty list literals
  / t : T                             ; Type annotation
  / l || r                            ; Boolean or
  / l + r                             ; Natural addition
  / l ++ r                            ; Text append
  / l # r                             ; List append
  / l && r                            ; Boolean and
  / l ∧ r                             ; Recursive record merge
  / l ⫽ r                             ; Non-recursive right-biased record merge
  / l ⩓ r                             ; Recursive record type merge
  / l * r                             ; Natural multiplication
  / l == r                            ; Boolean equality
  / l != r                            ; Boolean inequality
  / l === r                           ; Equivalence (using ASCII to avoid
                                      ; confusion with the equivalence judgment)
  / f a                               ; Function application
  / t.x                               ; Field selection
  / t.{ xs… }                         ; Field projection
  / t.(s)                             ; Field projection by type
  / T::r                              ; Record completion
  / assert : T                        ; Assert judgemental equality
  / e with k.ks… = v                  ; Nested record update
  / n.n                               ; Double-precision floating point literal
  / n                                 ; Natural number literal
  / ±n                                ; Integer literal
  / "s"                               ; Uninterpolated text literal
  / "s${t}ss…"                        ; Interpolated text literal
  / YYYY-MM-DD                        ; Date
  / hh:mm:ss                          ; Time
  / ±HH:MM                            ; Time zone
  / {}                                ; Empty record type
  / { k : T, ks… }                    ; Non-empty record type
  / {=}                               ; Empty record literal
  / { k = t, ks… }                    ; Non-empty record literal
  / <>                                ; Empty union type
  / < k : T | ks… >                   ; Union type with at least one non-empty
                                      ; alternative
  / < k | ks… >                       ; Union type with at least one empty
                                      ; alternative
  / showConstructor t                 ; Union to Text elimination
  / missing                           ; Identity for import alternatives,
                                      ; will always fail to resolve
  / l ? r                             ; Alternative imports resolution
  / https://authority directory file  ; URL import
  / directory file                    ; Absolute file path import
  / . directory file                  ; Relative file path import
  / .. directory file                 ; Relative file path import
  / ~ directory file                  ; Home-anchored file path import
  / env:x                             ; Environment variable import
  / Some a                            ; Constructor for a present Optional value

                                      ; Fixed symbols: never bindable, even when
                                      ; backquoted.  A de Bruijn index on these names
                                      ; is a syntax error.
  / Bool                              ; Bool type
  / Optional                          ; Optional type
  / Natural                           ; Natural type
  / Integer                           ; Integer type
  / Double                            ; Double type
  / Text                              ; Text type
  / Bytes                             ; Bytes type
  / List                              ; List type
  / True                              ; True term
  / False                             ; False term
  / Infinity                          ; Double infinity
  / -Infinity                         ; Double negative infinity
  / NaN                               ; Double NaN
  / None                              ; Absent Optional value
  / Type                              ; Type of terms
  / Kind                              ; Type of types
  / Sort                              ; Type of kinds
```

Names in source syntax fall into three classes:

* **Keywords** (`if`, `then`, `else`, `let`, `in`, `using`, `missing`, `as`,
  `merge`, `Some`, `toMap`, `assert`, `forall`, `with`,
  `showConstructor`) are part of the grammar.  They are not identifiers unless
  written in backquotes, which turns the keyword into an ordinary label that may
  be bound or used as a field or union constructor name.  Unquoted, keywords cannot
  be bound and also cannot be record field or union constructor
  names, except `Some`, which the grammar allows as a label via
  `any-label-or-some`. 

* **Fixed symbols** are the `Bool`, `Natural`, `None`, `Type`, … forms above,
  together with the `Double` literals `Infinity`, `-Infinity`, and `NaN` (like `True` /
  `False` for `Bool`).  They are values (or type-checking constants) but not
  identifiers: they cannot be bound (even if backquoted), and they cannot carry a De
  Bruijn index.  A quoted fixed symbol (`` `Bool` ``, `` `Infinity` ``) is still
  the same value.  Names of fixed symbols may be used as record field names or union
  constructor names without backticks.  `-Infinity` is not a label (it begins
  with `-`); the unbindable name is `Infinity`.

Note:  `Some` is currently a keyword (it introduces `Some a`); `None` is
  a fixed symbol. However, `Some` has fixed-symbol-like properties: it cannot be bound (even in backquotes), and it can be used without backquotes as a record field name or a union constructor name.

* **Built-in functions** (`Natural/isZero`, `List/length`, …) look like ordinary
  identifiers.  They may be bound (even without backquotes) and used as field or
  constructor names (with or without backquotes).  When such a name is **free** (De Bruijn
  index `0` at the top level, or the leftover index after peeling binders of
  that name), it denotes the built-in function of that name; see
  [type inference](./type-inference.md) and
  [β-normalization](./beta-normalization.md) for the list, types, and reduction
  rules for built-in functions.  Binding the name using `λ`, `∀`, or `let` will shadow the built-in function. In that case, the initial predefined meaning of the built-in function will remain
  available via a higher De Bruijn index, for example:
  `let List/length = "blah" in List/length@1 Natural [ 1, 2, 3 ]`.

  In the AST, a built-in function may be encoded simply as `Variable "Natural/isZero" 0`.
  In older versions of the Dhall standard, each built-in function had to be a special AST node in the syntax. A conforming implementation of Dhall may still use dedicated AST nodes for built-in functions but MUST treat those nodes as equivalent to the corresponding free variables.


```haskell
{-| This module contains the data types used to represent the syntax tree for
    a Dhall expression
-}

module Syntax
    ( -- * Types
      Expression(..)
    , Operator(..)
    , TextLiteral(..)
    , Builtin(..)
    , Constant(..)
    , ImportMode(..)
    , ImportType(..)
    , URL(..)
    , Scheme(..)
    , FilePrefix(..)
    , File(..)
    , PathComponent(..)
    , predefinedFunctionNames
    , predefinedFunctionTypes

      -- * Re-exports
    , Natural
    , Text
    ) where

import Crypto.Hash (Digest, SHA256)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Text (Text)
import Data.Word (Word8)
import Numeric.Natural (Natural)

import qualified Data.Map  as Map
import qualified Data.Time as Time

-- | Top-level type representing a Dhall expression
data Expression
    = Variable Text Natural
      -- ^ > x@n
      --
      -- Includes slash-names such as @Natural/isZero@ when they occur as
      -- identifiers.  A free @Variable "Natural/isZero" 0@ is the built-in
      -- function of that name.
    | Lambda Text Expression Expression
      -- ^ > λ(x : A) → b
    | Forall Text Expression Expression
      -- ^ > ∀(x : A) → B
    | Let Text (Maybe Expression) Expression Expression
      -- ^ > let x : A = a in b
      --   > let x     = a in b
    | If Expression Expression Expression
      -- ^ > if t then l else r
    | Merge Expression Expression (Maybe Expression)
      -- ^ > merge t u : T
      -- ^ > merge t u
    | ToMap Expression (Maybe Expression)
      -- ^ > toMap t : T
      -- ^ > toMap t
    | EmptyList Expression
      -- ^ > [] : T
    | NonEmptyList (NonEmpty Expression)
      -- ^ > [ t, ts… ]
    | Annotation Expression Expression
      -- ^ > t : T
    | Operator Expression Operator Expression
      -- ^ > l □ r
    | Application Expression Expression
      -- ^ > f a
    | Field Expression Text
      -- ^ > t.x
    | ProjectByLabels Expression [Text]
      -- ^ > t.{ xs… }
    | ProjectByType Expression Expression
      -- ^ > t.(s)
    | Completion Expression Expression
      -- ^ > T::r
    | Assert Expression
      -- ^ > assert : T
    | With Expression (NonEmpty PathComponent) Expression
      -- ^ > e with k.ks… = v
    | DoubleLiteral Double
      -- ^ > n.n
    | NaturalLiteral Natural
      -- ^ > n
    | IntegerLiteral Integer
      -- ^ > ±n
    | TextLiteral TextLiteral
      -- ^ > "s"
      --   > "s${t}ss…"  
    | BytesLiteral ByteString
      -- ^ > 0x"abcdef0123456789"
    | DateLiteral Time.Day
    | TimeLiteral
        Word8
        -- ^ Hour (0–23)
        Word8
        -- ^ Minute (0–59)
        Word8
        -- ^ Second, integer part (0–59)
        Integer
        -- ^ Fractional digits as a base-10 integer
        Int
        -- ^ Number of fractional digits (0 if there is no '.')
    | TimeZoneLiteral Time.TimeZone
    | RecordType [(Text, Expression)]
      -- ^ > {}
      --   > { k : T, ks… }
    | RecordLiteral [(Text, Expression)]
      -- ^ > {=}
      --   > { k = t, ks… }
    | UnionType [(Text, Maybe Expression)]
      -- ^ > <>
      --   > < k : T | ks… >
      --   > < k | ks… >
    | ShowConstructor Expression
      -- ^ > showConstructor t
    | Import ImportType ImportMode (Maybe (Digest SHA256))
    | Some Expression
      -- ^ > Some s
    | Builtin Builtin
    | Constant Constant
    deriving (Show)

-- | Associative binary operators
data Operator
    = Or                  -- ^ > ||
    | Plus                -- ^ > +
    | TextAppend          -- ^ > ++
    | ListAppend          -- ^ > #
    | And                 -- ^ > &&
    | CombineRecordTerms  -- ^ > ∧
    | Prefer              -- ^ > ⫽
    | CombineRecordTypes  -- ^ > ⩓
    | Times               -- ^ > *
    | Equal               -- ^ > ==
    | NotEqual            -- ^ > !=
    | Equivalent          -- ^ > ===
    | Alternative         -- ^ > ?
    deriving (Show)

{-| Data structure used to represent an interpolated @Text@ literal

    A @Text@ literal without any interpolations has an empty list.  For example,
    the @Text@ literal @\"foo\"@ is represented as:

    > TextLiteral [] "foo"

    A @Text@ literal with interpolations has one list element per interpolation.
    For example, the @Text@ literal @\"foo${x}bar${y}baz\"@ is represented as:

    > TextLiteral [("foo", Variable "x" 0), ("bar", Variable "y" 0)] "baz"
-}
data TextLiteral = Chunks [(Text, Expression)] Text
    deriving (Show)

-- | This instance comes in handy for implementing @Text@-related operations
instance Semigroup TextLiteral where
    Chunks xys₀ z₀ <> Chunks [] z₁ =
        Chunks xys₀ (z₀ <> z₁)
    Chunks xys₀ z₀ <> Chunks ((x₁, y₁) : xys₁) z₁ =
        Chunks (xys₀ <> ((z₀ <> x₁, y₁) : xys₁)) z₁

-- | This instance comes in handy for implementing @Text@-related operations
instance Monoid TextLiteral where
    mempty = Chunks [] ""

-- | Fixed symbols (never bindable, even when quoted)
data Builtin
    = Bool
    | Bytes
    | Date
    | Double
    | False
    | Integer
    | List
    | Natural
    | None
    | Optional
    | Text
    | Time
    | TimeZone
    | True
    deriving (Show)

-- | Type-checking constants
data Constant
    = Type
    | Kind
    | Sort
    deriving (Eq, Ord, Show)

-- | How to interpret the path to the import
data ImportMode
    = Code      -- ^ The default behavior: import the path as code to interpret
    | RawBytes  -- ^ @as Bytes@: import the path as raw bytes
    | RawText   -- ^ @as Text@: import the path as raw text
    | Location  -- ^ @as Location@: don't import and instead represent the path
                --   as a Dhall expression
    deriving (Show)

-- | Where to locate the import
data ImportType
    = Missing
        -- ^ > missing
    | Remote URL (Maybe Expression)
        -- ^ > https://authority directory file using headers
    | Path FilePrefix File
        -- ^ > /directory/file
        --   > ./directory/file
        --   > ../directory/file
        --   > ~/directory/file
    | Env Text
        -- ^ > env:x
    deriving (Show)

-- | Structured representation of an HTTP(S) URL
data URL = URL
    { scheme    :: Scheme
    , authority :: Text
    , path      :: File
    , query     :: Maybe Text
    }
    deriving (Show)

-- | The URL scheme
data Scheme
    = HTTP  -- ^ > http:\/\/
    | HTTPS -- ^ > https:\/\/
    deriving (Show)

-- | The anchor for a local filepath
data FilePrefix
    = Absolute  -- ^ @/@, an absolute path
    | Here      -- ^ @.@, a path relative to the current working directory
    | Parent    -- ^ @..@, a path relative to the parent working directory
    | Home      -- ^ @~@, a path relative to the user's home directory
    deriving (Show)

{-| Structured representation of a file path

    Note that the directory path components are stored in reverse order,
    meaning that the path @/foo\/bar\/baz@ is represented as:

    > File{ directory = [ "bar", "foo" ], file = "baz" }
-}
data File = File
    { directory :: [Text]  -- ^ Directory path components (in reverse order)
    , file :: Text         -- ^ File name
    }
    deriving (Show)

data PathComponent
    = Label Text
    | DescendOptional
    deriving (Show)

-- | Names of predefined functions.  A free @x@/@x@0 of one of these names
-- denotes the corresponding primitive.
predefinedFunctionNames :: [Text]
predefinedFunctionNames = Map.keys predefinedFunctionTypes

-- | Types of predefined functions, used when a free @x@0 is not bound in Γ.
predefinedFunctionTypes :: Map Text Expression
predefinedFunctionTypes = Map.fromList
    [ ("Natural/isZero"    , Builtin Natural --> Builtin Bool)
    , ("Natural/even"      , Builtin Natural --> Builtin Bool)
    , ("Natural/odd"       , Builtin Natural --> Builtin Bool)
    , ("Natural/toInteger" , Builtin Natural --> Builtin Integer)
    , ("Natural/show"      , Builtin Natural --> Builtin Text)
    , ("Natural/subtract"  , Builtin Natural --> Builtin Natural --> Builtin Natural)
    , ("Natural/build"     , naturalBuildType --> Builtin Natural)
    , ("Natural/fold"      , Builtin Natural --> naturalBuildType)
    , ("Integer/show"      , Builtin Integer --> Builtin Text)
    , ("Integer/toDouble"  , Builtin Integer --> Builtin Double)
    , ("Integer/negate"    , Builtin Integer --> Builtin Integer)
    , ("Integer/clamp"     , Builtin Integer --> Builtin Natural)
    , ("Double/show"       , Builtin Double --> Builtin Text)
    , ("Text/show"         , Builtin Text --> Builtin Text)
    , ("Text/replace"      , Forall "needle" (Builtin Text)
                               (Forall "replacement" (Builtin Text)
                                   (Forall "haystack" (Builtin Text) (Builtin Text))))
    , ("Date/show"         , Builtin Date --> Builtin Text)
    , ("Time/show"         , Builtin Time --> Builtin Text)
    , ("TimeZone/show"     , Builtin TimeZone --> Builtin Text)
    , ("List/build"        , Forall "a" (Constant Type) (listBuildBody --> listA))
    , ("List/fold"         , Forall "a" (Constant Type) (listA --> listBuildBody))
    , ("List/length"       , Forall "a" (Constant Type) (listA --> Builtin Natural))
    , ("List/head"         , Forall "a" (Constant Type) (listA --> optionalA))
    , ("List/last"         , Forall "a" (Constant Type) (listA --> optionalA))
    , ("List/reverse"      , Forall "a" (Constant Type) (listA --> listA))
    , ("List/indexed"      , Forall "a" (Constant Type)
                               (listA --> Application (Builtin List) indexedRecord))
    ]
  where
    infixr 5 -->
    a --> b = Forall "_" a b

    naturalBuildType =
        Forall "natural" (Constant Type)
            (Forall "succ" (Variable "natural" 0 --> Variable "natural" 0)
                (Forall "zero" (Variable "natural" 0) (Variable "natural" 0)))

    listA = Application (Builtin List) (Variable "a" 0)

    optionalA = Application (Builtin Optional) (Variable "a" 0)

    listBuildBody =
        Forall "list" (Constant Type)
            (Forall "cons" (Variable "a" 1 --> Variable "list" 0 --> Variable "list" 0)
                (Forall "nil" (Variable "list" 0) (Variable "list" 0)))

    indexedRecord =
        RecordType
            [ ("index", Builtin Natural)
            , ("value", Variable "a" 0)
            ]

```
