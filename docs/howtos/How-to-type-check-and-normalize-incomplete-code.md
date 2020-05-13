# How to type-check and normalize incomplete code

> Benefit from the language even when prototyping

You can type-check and normalize incomplete code by introducing a function argument named
`TODO` that has the following type:

```dhall
λ(TODO : ∀(a : Type) → a) → …
```

Then you can use `TODO` to fill any gaps in your code that have not been
implemented yet:

```dhall
  λ(TODO : ∀(a : Type) → a)
→ let List/map = https://prelude.dhall-lang.org/List/map

  let Text/concatSep = https://prelude.dhall-lang.org/Text/concatSep

  let Person = { name : Text, age : Natural }

  --  ↓ You can leave functions unimplemented
  let renderPerson = TODO (Person → Text)

  let renderList =
        λ(elements : List Text) → "[" ++ Text/concatSep ", " elements ++ "]"

  let renderPeople
      : List Person → Text
      = λ(people : List Person) →
          renderList (List/map Person Text renderPerson people)

  in  renderPeople
        [ { name = "John", age = 23 }
        , TODO Person  -- ← You can also leave values unimplemented
        , { name = "Mary", age = 24 }
        ]
```

The type `∀(a : Type) → a` is an impossible type that can never be created in Dhall,
so the `TODO` function argument can never be satisfied.  However, despite that we can
still type-check the function and normalize the function body:

```console
$ dhall --annotate <<< './example.dhall'
```
```dhall
  ( λ(TODO : ∀(a : Type) → a) →
          "["
      ++  (     TODO
                  ({ age : Natural, name : Text } → Text)
                  { age = 23, name = "John" }
            ++  ", "
            ++  TODO
                  ({ age : Natural, name : Text } → Text)
                  (TODO { age : Natural, name : Text })
            ++  ", "
            ++  TODO
                  ({ age : Natural, name : Text } → Text)
                  { age = 24, name = "Mary" }
          )
      ++  "]"
  )
: ∀(TODO : ∀(a : Type) → a) → Text
```
