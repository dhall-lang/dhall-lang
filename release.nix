let
  fetchNixpkgs = import ./nix/fetchNixpkgs.nix;

  nixpkgs = fetchNixpkgs {
    rev = "804060ff9a79ceb0925fe9ef79ddbf564a225d47";

    sha256 = "01pb6p07xawi60kshsxxq1bzn8a0y4s5jjqvhkwps4f5xjmmwav3";

    outputSha256 = "0ga345hgw6v2kzyhvf5kw96hf60mx5pbd9c4qj5q4nan4lr7nkxn";
  };

  overlay = pkgsNew: pkgsOld: {
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
  };

  pkgs = import nixpkgs { config = {}; overlays = [ overlay ]; };

  # Derivation that trivially depends on the current directory so that Hydra's
  # pull request builder always posts a GitHub status on each revision
  pwd = pkgs.runCommand "pwd" { here = ./.; } "touch $out";

in
  { all = pkgs.releaseTools.aggregate {
      name = "all";

      constituents = [
        pkgs.dhall-grammar
        pwd
      ];
    };
  }
