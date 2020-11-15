The following notation is a simplified version of the syntax found in
`./dhall.abnf`.  This simplified notation is used for all of the following
judgments:

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
    , are permitted and forbidden.
    a, b, f, l, r, e, t, u, A, B, E, T, U, c, i, o
      = x@n                               ; Identifier
                                          ; (`x` is short-hand for `x@0`)
      / λ(x : A) → b                      ; Anonymous function
      / ∀(x : A) → B                      ; Function type
                                          ; (`A → B` is short-hand for `∀(_ : A) → B`)
      / let x : A = a in b                ; Let expression with type annotation
      / let x     = a in b                ; Let expression without type annotation
      / if t then l else r                ; if-then-else expression
      / merge t u : T                     ; Union elimination with type annotation
      / merge t u                         ; Union elimination
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
      / {}                                ; Empty record type
      / { x : T, xs… }                    ; Non-empty record type
      / {=}                               ; Empty record literal
      / { x = t, xs… }                    ; Non-empty record literal
      / <>                                ; Empty union type
      / < x : T | xs… >                   ; Union type with at least one non-empty
                                          ; alternative
      / < x | xs… >                       ; Union type with at least one empty
                                          ; alternative
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
    
                                          ; Reserved identifiers for builtins
      / Natural/build                     ; Natural introduction
      / Natural/fold                      ; Natural elimination
      / Natural/isZero                    ; Test if zero
      / Natural/even                      ; Test if even
      / Natural/odd                       ; Test if odd
      / Natural/toInteger                 ; Convert Natural to Integer
      / Natural/show                      ; Convert Natural to Text representation
      / Natural/subtract                  ; Perform truncated subtraction on two Naturals
      / Integer/toDouble                  ; Convert Integer to Double
      / Integer/show                      ; Convert Integer to Text representation
      / Integer/negate                    ; Invert sign of Integers, with positive
                                          ; values becoming negative and vice-versa
      / Integer/clamp                     ; Convert Integer to Natural by clamping
                                          ; negative values to zero
      / Double/show                       ; Convert Double to Text representation
      / List/build                        ; List introduction
      / List/fold                         ; List elimination
      / List/length                       ; Length of list
      / List/head                         ; First element of list
      / List/last                         ; Last element of list
      / List/indexed                      ; Tag elements with index
      / List/reverse                      ; Reverse list
      / Text/show                         ; Convert Text to its own representation
      / Text/replace                      ; Replace a section of a Text literal
      / Bool                              ; Bool type
      / Optional                          ; Optional type
      / Natural                           ; Natural type
      / Integer                           ; Integer type
      / Double                            ; Double type
      / Text                              ; Text type
      / List                              ; List type
      / True                              ; True term
      / False                             ; False term
      / None                              ; Absent Optional value
      / Type                              ; Type of terms
      / Kind                              ; Type of types
      / Sort                              ; Type of kinds

```agda
module Syntax where

open import Agda.Builtin.Float  using (Float)
open import Agda.Builtin.Int    using (Int)
open import Agda.Builtin.Nat    using (Nat)
open import Agda.Builtin.String using (String)
open import Data.Maybe          using (Maybe)
open import Data.List.NonEmpty  using (List⁺)
open import Data.Product
open import Data.List           hiding (List)

import Data.List

postulate
  Digest : Set  -- A SHA256 digest

data Expression : Set

data Operator : Set where
    Or                 : Operator  -- ||
    Plus               : Operator  -- +
    TextAppend         : Operator  -- ++
    ListAppend         : Operator  -- #
    And                : Operator  -- &&
    CombineRecordTerms : Operator  -- ∧
    Prefer             : Operator  -- ⫽
    CombineRecordTypes : Operator  -- ⩓
    Times              : Operator  -- *
    Equal              : Operator  -- ==
    NotEqual           : Operator  -- !=
    Equivalent         : Operator  -- ===
    Alternative        : Operator  -- ?

{-| Data structure used to represent an interpolated `Text` literal

    A `Text` literal without any interpolations has an empty list.  For example,
    the `Text` literal `"foo"` is represented as:

    record { init = []; last = "foo" }

    A `Text` literal with interpolations has one list element per interpolation.
    For example, the `Text` literal `"foo${x}bar${y}baz"` is represented as:

    record { init = ("foo", Variable "x" 0) ∷ ("bar", Variable "y" 0) ∷ []; last = "baz" }
-}
record Chunks : Set where
    inductive
    field
      init : Data.List.List (String × Expression)
      last : String

record File : Set where
  field
    directory : Data.List.List String
    file      : String

data FilePrefix : Set where
    Absolute : FilePrefix
    Here     : FilePrefix
    Parent   : FilePrefix
    Home     : FilePrefix

data ImportMode : Set where
    Code     : ImportMode
    RawText  : ImportMode
    Location : ImportMode

data Scheme : Set where
    HTTP  : Scheme
    HTTPS : Scheme

record URL : Set where
    field
      scheme    : Scheme
      authority : String
      path      : File
      query     : Maybe String
      headers   : Maybe String

data ImportType : Set where
    Missing : ImportType
    Remote  : URL → ImportType
    Path    : FilePrefix → File → ImportType
    Env     : String → ImportType

data BuiltinName : Set where
    Natural/build     : BuiltinName
    Natural/fold      : BuiltinName
    Natural/isZero    : BuiltinName
    Natural/even      : BuiltinName
    Natural/odd       : BuiltinName
    Natural/toInteger : BuiltinName
    Natural/show      : BuiltinName
    Natural/subtract  : BuiltinName
    Integer/toDouble  : BuiltinName
    Integer/show      : BuiltinName
    Integer/negate    : BuiltinName
    Integer/clamp     : BuiltinName
    Double/show       : BuiltinName
    List/build        : BuiltinName
    List/fold         : BuiltinName
    List/length       : BuiltinName
    List/head         : BuiltinName
    List/last         : BuiltinName
    List/indexed      : BuiltinName
    List/reverse      : BuiltinName
    Text/show         : BuiltinName
    Text/replace      : BuiltinName
    Bool              : BuiltinName
    Optional          : BuiltinName
    Natural           : BuiltinName
    Integer           : BuiltinName
    Double            : BuiltinName
    Text              : BuiltinName
    List              : BuiltinName
    True              : BuiltinName
    False             : BuiltinName
    None              : BuiltinName

data Level : Set where
    Type : Level
    Kind : Level
    Sort : Level

data Expression where
    Variable : String → Nat → Expression
    -- x@n
    Lambda : String → Expression → Expression
    -- λ(x : A) → b
    Forall : String → Expression → Expression
    -- ∀(x : A) → B
    Let : String → Maybe Expression → Expression → Expression
    -- let x : A = a in b
    -- let x     = a in b
    If : Expression → Expression → Expression → Expression
    -- if t then l else r
    Merge : Expression → Expression → Expression → Expression
    -- merge t u : T
    -- merge t u
    EmptyList : Expression → Expression
    -- [] : T
    NonEmptyList : List⁺ Expression → Expression
    -- [ t, ts… ]
    Annotation : Expression → Expression → Expression
    -- t : T
    BinaryOperator : Expression → Operator → Expression → Expression
    -- l □ r
    Application : Expression → Expression → Expression
    -- f a
    Field : Expression → String → Expression
    -- t.x
    ProjectByLabels : Expression → Data.List.List String → Expression
    -- t.{ xs… }
    ProjectByType : Expression → Expression → Expression
    -- t.(s)
    Completion : Expression → Expression → Expression
    -- T::r
    Assert : Expression → Expression
    -- assert : T
    With : Expression → List⁺ Expression → Expression → Expression
    -- e with k.ks… = v
    DoubleLiteral : Float → Expression
    -- n.n
    NaturalLiteral : Nat → Expression
    -- n
    IntegerLiteral : Int → Expression
    -- ±n
    TextLiteral : Chunks → Expression
    -- "s"
    -- "s${t}ss…"
    RecordType : Data.List.List (String × Expression) → Expression
    -- {}
    -- { x : T, xs… }
    RecordLiteral : Data.List.List (String × Expression) → Expression
    -- {=}
    -- { x = t, xs… }
    UnionType : Data.List.List (String × Maybe Expression) → Expression
    -- <>
    -- < x : T | xs… >
    -- < x | xs… >
    Import : ImportType → ImportMode → Maybe Digest → Expression
    Some : Expression → Expression
    -- Some s
    Builtin : BuiltinName → Expression
    Constant : Level → Expression
```
