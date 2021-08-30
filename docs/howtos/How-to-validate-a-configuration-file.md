# How to validate a configuration file

This guide illustrates how you can valid a configuration file's value (as
opposed to only validating the type) through the use of the `assert` keyword.

We'll begin from the following example configuration type:

```dhall
{ cpus : Natural
, minReplicas : Natural
, maxReplicas : Natural
}
```

We would like to not only validate that a configuration file has that type, but
we'd also like to validate that the configuration file has the correct value.
For example, we might want to verify that:

* The `cpus` field can only be a value in between 1 and 4

* The `minReplicas` field should be less than or equal to the `maxReplicas`
  field

The following complete example shows how to do so:

```dhall
let Prelude =
      https://prelude.dhall-lang.org/v20.2.0/package.dhall
        sha256:a6036bc38d883450598d1de7c98ead113196fe2db02e9733855668b18096f07b

let Config =
      { cpus : Natural
      , minReplicas : Natural
      , maxReplicas : Natural
      }

let example
    : Config
    = { cpus = 2
      , minReplicas = 1
      , maxReplicas = 3
      }

let validate =
      \(config : Config) ->
        let expected =
              { validCPUs = True
              , validReplicas = True
              }

        let actual =
              { validCPUs =
                      Prelude.Natural.lessThanEqual 1 config.cpus
                  &&  Prelude.Natural.lessThanEqual config.cpus 4
              , validReplicas =
                  Prelude.Natural.lessThanEqual
                    config.minReplicas
                    config.maxReplicas
              }

        in  expected === actual

let _ = assert : validate example

in  example
```

In other words:

* Create a validation function that compares expected values against actual
  values

  e.g. the `validate` function in the above example

  This validation function uses the equivalence operator (i.e. `===` / `≡`) to
  compare the expected and actual values, but omits the `assert` keyword, which
  is deferred until later.

  If you need to validate multiple values, then compare two records with one
  field each per validation.

* Verify the validation function against the sample configuration using `assert`

  e.g. `assert : validate example`

This validation still occurs at type-checking time because that is when the
`assert` keyword is checked, so these sorts of value-level validations can still
be used for ahead-of-time error detection.

If one or more validations fail, the error message will display only the
validations that failed.  For example, if we change the `cpus` field to 8 then
we would get this error message:

```
Use "dhall --explain" for detailed errors

Error: Assertion failed

{ validCPUs = - True
              + False
, …
}

37│         assert : validate example
38│
```

… indicating that the CPU validation failed.

If you are creating a package for others to consume then you can distribute
the validation function as part of the package.  However, the main limitation of
this approach is that you will need to instruct the users of your package to use
the validation since the language does not currently support automatically
associating a validation with the corresponding type.
