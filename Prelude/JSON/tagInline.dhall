--| Prepare a union value for JSON- or YAML-encoding with the inline layout
let Nesting =
        ./Nesting.dhall
          sha256:6284802edd41d5d725aa1ec7687e614e21ad1be7e14dd10996bfa9625105c335
      ? ./Nesting.dhall

let Tagged =
        ./Tagged.dhall
          sha256:21feca7d2b23f210d0696131d792e18a7d24fdcc85d41a49ba85b98670eba194
      ? ./Tagged.dhall

let tagInline
    : Text → ∀(a : Type) → a → Tagged a
    = λ(tagFieldName : Text) →
      λ(a : Type) →
      λ(contents : a) →
        { nesting = Nesting.Inline, field = tagFieldName, contents }

let example0 =
      let Example = < Left : { foo : Natural } | Right : { bar : Bool } >

      in    assert
          :   tagInline "name" Example (Example.Left { foo = 2 })
            ≡ { field = "name"
              , nesting = Nesting.Inline
              , contents = Example.Left { foo = 2 }
              }

in  tagInline
