{ src ? { rev = ""; }, ... }:

let
  nixpkgs = import ./nixops/nixpkgs.nix;

  overlay = import ./nixops/overlay.nix;

  config = {
    permittedInsecurePackages = [
      "python2.7-cryptography-2.9.2"
    ];
  };

  pkgs = import nixpkgs { inherit config; overlays = [ overlay ]; };

  # Derivation that trivially depends on the input source code revision.
  # As this is included in the "dhall-lang" aggregate, it forces every
  # commit to have a corresponding GitHub status check, even if the
  # commit doesn't make any changes (which can happen when merging
  # master in).
  rev = pkgs.runCommand "rev" {} ''echo "${src.rev}" > $out'';

  machine =
    (import "${nixpkgs}/nixos" {
      configuration = {
        imports = [ ./nixops/logical.nix ./nixops/physical.nix ];

        networking.hostName = "dhall-lang";
      };

      system = "x86_64-linux";
    }).system;

  inherit (pkgs.haskellPackages) standard;

  vm =
    (import "${nixpkgs}/nixos" {
      configuration = {
        imports = [
          ./nixops/logical.nix
          ./nixops/physical.nix
          "${nixpkgs}/nixos/modules/virtualisation/qemu-vm.nix"
        ];

        networking.hostName = "dhall-lang";

        systemd.services.self-deploy.enable = false;

        users = {
          mutableUsers = false;

          users.root.password = "";
        };

        virtualisation = {
          cores = 2;

          memorySize = 4096;
        };
      };

      system = "x86_64-linux";
    }).vm;

in
  { dhall-lang = pkgs.releaseTools.aggregate {
      name = "dhall-lang";

      constituents = [
        pkgs.dhall-grammar
        pkgs.ensure-trailing-newlines
        pkgs.prelude-lint
        pkgs.test-files-lint
        standard
        machine
        vm
        rev
      ];
    };

    inherit (pkgs) expected-prelude expected-test-files docs website store;

    inherit machine vm standard;
  }
