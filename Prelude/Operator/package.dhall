{-
Note: This package does not use one file per function because the operator
names contain symbols such as `*` that may cause problems.
-}
let {-
        `+` m n

    computes `m + n`
    -} `+`
    : Natural → Natural → Natural
    = λ(m : Natural) → λ(n : Natural) → m + n

let example1 = assert : `+` 2 1 ≡ 3

let {-
        `*` m n

    computes `m * n`
    -} `*`
    : Natural → Natural → Natural
    = λ(m : Natural) → λ(n : Natural) → m * n

let example2 = assert : `*` 21 2 ≡ 42

let {-
        `++` m n

    computes `m ++ n`
    -} `++`
    : Text → Text → Text
    = λ(m : Text) → λ(n : Text) → m ++ n

let example3 = assert : `++` "Hello" "Dhall" ≡ "HelloDhall"

let {-
        `#` Type m n

    computes `m # n`
    -} `#`
    : ∀(type : Type) → List type → List type → List type
    = λ(type : Type) → λ(m : List type) → λ(n : List type) → m # n

let example4 = assert : `#` Natural [ 1, 2 ] [ 3 ] ≡ [ 1, 2, 3 ]

let {-
        `==` m n

    computes `m == n`
    -} `==`
    : Bool → Bool → Bool
    = λ(m : Bool) → λ(n : Bool) → m == n

let example5 = assert : `==` True False ≡ False

let {-
        `!=` m n

    computes `m != n`
    -} `!=`
    : Bool → Bool → Bool
    = λ(m : Bool) → λ(n : Bool) → m != n

let example6 = assert : `!=` True False ≡ True

let {-
        `&&` m n

    computes `m && n`
    -} `&&`
    : Bool → Bool → Bool
    = λ(m : Bool) → λ(n : Bool) → m && n

let example7 = assert : `&&` False True ≡ False

let {-
        `||` m n

    computes `m || n`
    -} `||`
    : Bool → Bool → Bool
    = λ(m : Bool) → λ(n : Bool) → m || n

let example8 = assert : `||` False True ≡ True

in  { `+`, `*`, `++`, `#`, `==`, `!=`, `&&`, `||` }
