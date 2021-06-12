{-|
This is a convenient type-level function when using `dhall-to-json`'s support
for preserving alternative names

For example, this code:

```
let map = ../List/map

let Provisioner =
      < shell :
          { inline : List Text }
      | file :
          { source : Text, destination : Text }
      >

let Tagged = ./Tagged

let Nesting = ./Nesting

let wrap
    : Provisioner → Tagged Provisioner
    = λ(x : Provisioner) →
        { field = "type", nesting = Nesting.Nested "params", contents = x }

in  { provisioners =
        map
        Provisioner
        (Tagged Provisioner)
        wrap
        [ Provisioner.shell { inline = [ "echo foo" ] }
        , Provisioner.file
          { source = "app.tar.gz", destination = "/tmp/app.tar.gz" }
        ]
    }
```

... produces this JSON:

```
{
  "provisioners": [
    {
      "params": {
        "inline": [
          "echo foo"
        ]
      },
      "type": "shell"
    },
    {
      "params": {
        "destination": "/tmp/app.tar.gz",
        "source": "app.tar.gz"
      },
      "type": "file"
    }
  ]
}
```

-}
let Tagged
    : Type → Type
    = λ(a : Type) →
        { field : Text
        , nesting :
              ./Nesting.dhall
                sha256:6284802edd41d5d725aa1ec7687e614e21ad1be7e14dd10996bfa9625105c335
            ? ./Nesting.dhall
        , contents : a
        }

in  Tagged
