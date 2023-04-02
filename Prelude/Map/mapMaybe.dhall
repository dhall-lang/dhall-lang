--| Apply a function across the values of a `Map k v`, dropping
-- entries whose values return `None`.
let Map/empty =
        missing
          sha256:4c612558b8bbe8f955550ed3fb295d57b1b864c85cd52615b52d0ee0e9682e52
      ? ./empty.dhall

let Map/unpackOptionals =
        missing
          sha256:66c3e6f6f81418cf99342e1dba739617c01af4b27c1ca5e2e1d7bce64a522e22
      ? ./unpackOptionals.dhall

let Map/map =
        missing
          sha256:23e09b0b9f08649797dfe1ca39755d5e1c7cad2d0944bdd36c7a0bf804bde8d0
      ? ./map.dhall

let Map/Type =
        missing
          sha256:210c7a9eba71efbb0f7a66b3dcf8b9d3976ffc2bc0e907aadfb6aa29c333e8ed
      ? ./Type.dhall

let mapMaybe
    : ∀(k : Type) →
      ∀(a : Type) →
      ∀(b : Type) →
      (a → Optional b) →
      Map/Type k a →
        Map/Type k b
    = λ(k : Type) →
      λ(a : Type) →
      λ(b : Type) →
      λ(f : a → Optional b) →
      λ(m : Map/Type k a) →
        Map/unpackOptionals k b (Map/map k a (Optional b) f m)

let property =
      λ(k : Type) →
      λ(a : Type) →
      λ(b : Type) →
      λ(f : a → Optional b) →
        assert : mapMaybe k a b f (Map/empty k a) ≡ Map/empty k b

let example0 =
        assert
      :   mapMaybe
            Text
            Natural
            Text
            ( λ(n : Natural) →
                if Natural/isZero n then None Text else Some (Natural/show n)
            )
            (toMap { foo = 2, bar = 0 })
        ≡ toMap { foo = "2" }

in  mapMaybe
