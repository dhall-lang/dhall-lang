--| Build a Text by copying the given Text the specified number of times
let concat =
        ./concat.dhall
          sha256:731265b0288e8a905ecff95c97333ee2db614c39d69f1514cb8eed9259745fc0
      ? ./concat.dhall

let List/replicate =
        ../List/replicate.dhall
          sha256:d4250b45278f2d692302489ac3e78280acb238d27541c837ce46911ff3baa347
      ? ../List/replicate.dhall

let replicate
    : Natural → Text → Text
    = λ(num : Natural) → λ(text : Text) → concat (List/replicate num Text text)

let example0 = assert : replicate 3 "foo" ≡ "foofoofoo"

let property = λ(text : Text) → assert : replicate 0 text ≡ ""

in  replicate
