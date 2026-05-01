--| `lessThanEqual` checks if one Natural is less than or equal to another.
let equal =
        missing
          sha256:d7d98753b1551f1dbd51b09231ffe4d000a74f76f3a9457990bf7bb4f640ff5e
      ? ./equal.dhall

let lessThan =
        missing
          sha256:11d199975930afe0a666095d43948588d4bc4aa52c4fd8dbbfe68c72dd3f1908
      ? ./lessThan.dhall

let lessThanEqual
    : Natural → Natural → Bool
    = λ(x : Natural) → λ(y : Natural) → lessThan x y || equal x y

let example0 = assert : lessThanEqual 5 6 ≡ True

let example1 = assert : lessThanEqual 5 5 ≡ True

let example2 = assert : lessThanEqual 5 4 ≡ False

let property0 = λ(n : Natural) → assert : lessThanEqual n n ≡ True

in  lessThanEqual
