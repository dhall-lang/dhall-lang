{-|
Returns `True` for any `Integer` less than `+0`.

`negative` is more efficient than `./lessThan +0` or `./lessThanEqual -1`.
-}
let positive =
        ./positive.dhall
          sha256:7bdbf50fcdb83d01f74c7e2a92bf5c9104eff5d8c5b4587e9337f0caefcfdbe3
      ? ./positive.dhall

let negative
    : Integer → Bool
    = λ(n : Integer) → positive (Integer/negate n)

let example0 = assert : negative +1 ≡ False

let example1 = assert : negative +0 ≡ False

let example2 = assert : negative -1 ≡ True

in  negative
