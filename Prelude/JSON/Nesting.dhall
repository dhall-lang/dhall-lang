{-|
This type is used as part of `dhall-json`'s support for preserving alternative
names

For example, this Dhall code:

```
let Example = < Left : { foo : Natural } | Right : { bar : Bool } >

let Nesting = < Inline | Nested : Text >

in  { field =
        "name"
    , nesting =
        Nesting.Inline
    , contents =
        Example.Left { foo = 2 }
    }
```

... generates this JSON:

```
{
  "foo": 2,
  "name": "Left"
}
```

-}
let Nesting
    : Type
    = < Inline | Nested : Text >

in  Nesting
