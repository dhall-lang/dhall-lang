{ src ? { rev = ""; }, ... }:

let
  nixpkgs =
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/55b8860aa209e987f6f15c523811e4861d97d6af.tar.gz";

      sha256 = "0ri58704vwv6gnyw33vjirgnvh2f1201vbflk0ydj5ff7vpyy7hf";
    };

  overlay = pkgsNew: pkgsOld: {
    dhall =
      let
        json = builtins.fromJSON (builtins.readFile ./nixops/dhall-haskell.json);

        dhall-haskell =
          pkgs.fetchFromGitHub {
            owner = "dhall-lang";

            repo = "dhall-haskell";

            inherit (json) rev sha256 fetchSubmodules;
          };

        dhall-haskell-derivations =
          import "${dhall-haskell}/default.nix";

      in
        dhall-haskell-derivations.dhall;

    instaparse-check = pkgsNew.writeText "build.boot" ''
      (require '[instaparse.core :refer [parser]])

      (def grammar (slurp "${./standard/dhall.abnf}"))

      (def dhall-parser
        (parser grammar
                :input-format :abnf
                :start :complete-expression
                :output-format :hiccup))

      (deftask check []
        (println "Grammar is syntactically correct."))
    '';

    dhall-grammar =
      pkgsNew.runCommand
        "instaparse-accepts-grammar"
        { nativeBuildInputs = [
            pkgsNew.boot
          ];
        }
        ''
          export _JAVA_OPTIONS=-Duser.home="''${PWD}"
          export BOOT_CLOJURE_VERSION=1.8.0
          export BOOT_VERSION=2.7.2
          export BOOT_HOME="''${PWD}"

          cp ${pkgsNew.instaparse-check} build.boot
          boot -d instaparse:1.4.9 check

          touch $out
        '';

    expected-diagnostic-files =
      pkgsNew.runCommand "expected-diagnostic-files" {} ''
        ${pkgsNew.rsync}/bin/rsync --archive ${./tests}/ "$out"

        ${pkgsNew.coreutils}/bin/chmod --recursive u+w "$out"

        for FILE in $(${pkgsNew.findutils}/bin/find "$out" -type f -name '*.dhallb'); do
          ${pkgsNew.cbor-diag}/bin/cbor2diag.rb "$FILE" > "''${FILE%.dhallb}.diag"
        done
      '';

    expected-prelude = pkgsNew.runCommand "expected-prelude" {} ''
      ${pkgsNew.rsync}/bin/rsync --archive ${./Prelude}/ "$out"

      ${pkgsNew.coreutils}/bin/chmod --recursive u+w "$out"

      for FILE in $(${pkgsNew.findutils}/bin/find "$out" -type f); do
        ${pkgsNew.dhall}/bin/dhall lint --inplace "$FILE"
        XDG_CACHE_HOME=/var/empty ${pkgsNew.dhall}/bin/dhall freeze --all --cache --inplace "$FILE"
      done
    '';

    diagnostic-files-lint = pkgsNew.runCommand "diagnostic-files-lint" {} ''
      ${pkgsNew.rsync}/bin/rsync --archive ${pkgsNew.expected-diagnostic-files}/ ./tests.expected
      ${pkgsNew.rsync}/bin/rsync --archive ${./tests}/ ./tests.actual

      ${pkgsNew.diffutils}/bin/diff --recursive ./tests.{actual,expected}

      touch $out
    '';

    prelude-lint = pkgsNew.runCommand "prelude-lint" {} ''
      ${pkgsNew.rsync}/bin/rsync --archive ${pkgsNew.expected-prelude}/ ./Prelude.expected
      ${pkgsNew.rsync}/bin/rsync --archive ${./Prelude}/ ./Prelude.actual

      ${pkgsNew.diffutils}/bin/diff --recursive ./Prelude.{actual,expected}

      touch $out
    '';

    ensure-trailing-newlines = pkgsNew.runCommand "ensure-trailing-newlines" {} ''
      for FILE in $(${pkgsNew.findutils}/bin/find ${./tests} -type f -name '*.dhall'); do
        LAST_CHARACTER=$(${pkgsNew.coreutils}/bin/tail --bytes 1 "$FILE")

        if [ ! -s "''${FILE}" -o "$LAST_CHARACTER" != "" ]; then
          echo "''${FILE} is missing a trailing newline"

          exit 1
        fi
      done

      touch $out
    '';
  };

  pkgs = import nixpkgs { config = {}; overlays = [ overlay ]; };

  # Derivation that trivially depends on the input source code revision.
  # As this is included in the "dhall-lang" aggregate, it forces every
  # commit to have a corresponding GitHub status check, even if the
  # commit doesn't make any changes (which can happen when merging
  # master in).
  rev = pkgs.runCommand "rev" {} ''echo "${src.rev}" > $out'';

in
  { dhall-lang = pkgs.releaseTools.aggregate {
      name = "dhall-lang";

      constituents = [
        pkgs.dhall-grammar
        pkgs.ensure-trailing-newlines
        pkgs.prelude-lint
        pkgs.diagnostic-files-lint
        rev
      ];
    };

    inherit (pkgs) expected-prelude expected-diagnostic-files;
  }
