# Dhall In Production

> Example commercial use cases for Dhall

This section covers confirmed uses of Dhall in production, including official integrations with other tools/ecosystems and use within companies.  You can open an issue if you would like to list your company or tool here:

## Integrations

### [Eta](https://eta-lang.org/)

The Eta programming language supports Dhall for configuring their build tool, Etlas

### [PureScript Package Sets](https://github.com/purescript/package-sets)

PureScript's default global package set is defined as a large Dhall configuration file that can be locally overridden

## Companies

### [Advanced Telematic Systems GmbH](https://www.atsgarage.com/)

Open sourced their [Dhall bindings to GoCD](https://github.com/advancedtelematic/gocd-dhall-example)

### [AlasConnect](http://alasconnect.com/)

Uses Haskell bindings to Dhall to configure application servers

### [Awake Security](https://awakesecurity.com/)

Uses Haskell bindings to Dhall to simplify redundant appliance cluster configurations

### [Cachix](https://github.com/cachix/cachix)

Uses `dhall-haskell` to persist user configuration between command line interface invocations.

### [CircuitHub](https://circuithub.com/)

Uses Dhall to configure command-line applications (i.e. as an alternative to a JSON configuration)

### [Coralogix](https://coralogix.com/)

Open sourced their [Dhall bindings to Concourse CI](https://github.com/coralogix/dhall-concourse)

### [Earnest Research](https://www.earnestresearch.com/)

Uses `dhall` and `dhall-kubernetes` to configure their Kubernetes infrastructure and
open-sourced their bindings as [`dhall-packages`](https://github.com/EarnestResearch/dhall-packages)

### [Formation](https://formation.ai/)

Uses Dhall customized with additional built-ins as a serializable domain-specific language

### [IOHK](https://iohk.io/)

Uses `dhall-to-yaml` to simplify redundant runtime configuration

### Kos Media, LLC

Uses `dhall`, `dhall-json`, and `dhall-nix` to convert Dhall definitions for microservice configs and encrypted secrets to various formats including JSON, YAML, and Nix in their delivery pipeline (currently into AWS).

### [KSF Media](https://github.com/KSF-Media)

Uses `dhall-kubernetes`, `dhall-to-yaml` and `dhall-to-text` to configure, modularize and template Terraform and Kubernetes configurations

### [meshcloud GmbH](https://www.meshcloud.io)

Uses Dhall as the primary configuration model for its multi-cloud management software [meshStack](https://docs.meshcloud.io/docs/meshstack.configuration.html). The product documentation is [open source](https://github.com/meshcloud/meshcloud-docs) and includes a tool to automatically synchronise code snippets between the Dhall configuration model and product documentation.

meshcloud GmbH is also the first corporate sponsor for [Dhall on opencollective](https://opencollective.com/dhall/).

### [Mira Networks](http://www.miranetworks.net)

Uses `dhall-haskell` to define logs for parsing. The software generates a specific log parser in either Python, Haskell, Elm or Erlang.

### [NoRedInk](https://t.co/FTLTeyzykY)

Uses `dhall` to configure their build process
