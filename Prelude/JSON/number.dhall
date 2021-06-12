{-|
Create a JSON number from a Dhall `Double`

```
let JSON = ./package.dhall
in  JSON.render (JSON.number 42.0)
= "42.0"

let JSON = ./package.dhall
in  JSON.render (JSON.number -1.5e-10)
= "-1.5e-10"
```
-}
let JSON =
        ./Type.dhall
          sha256:40edbc9371979426df63e064333b02689b969c4cfbbccfa481216d2d1a6e9759
      ? ./Type.dhall

let double =
        ./double.dhall
          sha256:e70162c73c4978ad0d0d99505f61c7d990f3abadfcc08b34388b29c0934a7a32
      ? ./double.dhall

let number
    : Double → JSON
    = double

in  number
