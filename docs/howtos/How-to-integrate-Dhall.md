# How to integrate Dhall

> Quickly survey options for reading in Dhall configuration files

You can integrate the Dhall configuration language into your project in one of the following three ways:

* Language support for reading Dhall configuration files (this is the only way to get _complete_ support for Dhall, including invoking Dhall functions from your application)
* An external `dhall-to-${FORMAT}` executable that transforms Dhall into another configuration format
* The `dhall text` command combined with a pure Dhall function that renders your desired file format

## Language support

The following languages can read in Dhall configuration files directly using a package that natively implements Dhall:

* [Clojure](https://github.com/f-f/dhall-clj) - Via the `dhall-clj` package
* [Haskell](https://github.com/dhall-lang/dhall-haskell/blob/master/dhall/README.md) - Via the `dhall` package
* [Go](https://github.com/philandstuff/dhall-golang) - Via the `dhall-golang` package
* [Ruby](https://git.sr.ht/~singpolyma/dhall-ruby) - Via the `dhall-ruby` package
* [Rust](https://crates.io/crates/serde_dhall) - Via the `serde_dhall` package

The following languages can read in Dhall configuration files by building on top of one of the above three implementations:

* [Eta](https://github.com/eta-lang/dhall-eta) - Via the Haskell implementation
* [Java](https://github.com/eta-lang/dhall-eta) - The above Eta implementation also provides Java bindings
* [Nix](https://github.com/dhall-lang/dhall-haskell/tree/master/dhall-nix) - Via the Haskell implementation

The following language bindings are currently in progress:

* [PureScript](https://github.com/MonoidMusician/dhall-purescript/blob/master/README.md) - Via the `dhall-purescript` package

The following integrations built on top of another implementation are still in progress:

* [C](https://github.com/as-capabl/clay-dhall/blob/master/README.md) - Using the Haskell package as a shared library

## External executable

You can convert Dhall to one of the following configuration formats if your language does not natively support Dhall.

* [YAML](https://github.com/dhall-lang/dhall-haskell/blob/master/dhall-yaml/README.md) - Via the `dhall-to-yaml-ng` executable

* [JSON](https://github.com/dhall-lang/dhall-haskell/blob/master/dhall-json/README.md) - Via the `dhall-to-json` executable

* [XML](https://git.sr.ht/~singpolyma/dhall-xml-ruby) - Via the `dhall-to-xml` executable

* TOML - Via the `dhall-to-json` executable

  Dhall does not yet support conversion to TOML, but until then you may be able to work around this
  by using the fact that TOML is a superset of JSON (assuming that you do not care about the appearance
  of the intermediate configuration file).

* [Bash](https://github.com/dhall-lang/dhall-haskell/blob/master/dhall-bash/README.md) - Via the `dhall-to-bash` executable

  Technically not a "configuration format", but many programs are configured via Bash scripts

* [Kubernetes](https://github.com/dhall-lang/dhall-kubernetes/blob/master/README.md) - Via the `dhall-to-yaml` executable

  `dhall-kubernetes` is a repository containing Kubernetes-related Dhall types and functions.  However, you still need to use `dhall-to-yaml` to generate the YAML configuration that Kubernetes expects since Kubernetes is not Dhall-aware

## Render within Dhall

"Render within Dhall" is a special case of "External executable" where the external executable is the `dhall text` command (i.e. the `text` subcommand of the `dhall` executable).  The difference is that the logic to generate a given configuration file format is implemented within Dhall and then the expression is interpreted and displayed using the `dhall text` command.

This can just mean templating an existing format, like markdown:

```dhall
-- ./template.dhall

let Prelude = https://prelude.dhall-lang.org/package.dhall

let toItem = \(item : Text) -> "* ${item}"

let toList = Prelude.Text.concatMapSep "\n" Text toItem

let fruits = [ "apple", "orange", "banana" ]

in  ''
    # About me

    These are my favorite fruits:

    ${toList fruits}
    ''
```

```console
$ dhall text --file ./template.dhall
# About me

These are my favorite fruits:

* apple
* orange
* banana
```

... or you can provide a more structured integration implemented in "pure Dhall".  For example, the Prelude natively supports rendering well-formed JSON using the same `dhall text` command:

```dhall
-- ./json.dhall

let JSON = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/JSON/package.dhall

in  JSON.render (JSON.array [ JSON.number 1.0, JSON.bool True ])
```

```console
$ dhall text --file './json.dhall'
[ 1.0, true ]
```

"Pure Dhall" integrations include:

* [JSON](https://github.com/dhall-lang/dhall-lang/blob/master/Prelude/JSON/render) - Via the Dhall Prelude

* [XML](https://github.com/dhall-lang/dhall-lang/blob/master/Prelude/XML/render) - Via the Dhall Prelude

* [NetHack](https://github.com/dhall-lang/dhall-nethack) - Via the `dhall-nethack` repository
