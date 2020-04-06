{ src ? { rev = ""; }, ... }:

let
  nixpkgs =
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/fc7efd51d616858af206d5c3e33ebf4b77487e38.tar.gz";

      sha256 = "1n75xcad22bmp4x7vzrfwqblvq0z0x7xkmn0id9rj14qy2vlkq6d";
    };

  dhallLangNixpkgs = import ./nixops/dhallLangNixpkgs.nix;

  overlay = pkgsNew: pkgsOld: {
    dhall-haskell-derivations =
      let
        json = builtins.fromJSON (builtins.readFile ./nixops/dhall-haskell.json);

        dhall-haskell =
          pkgs.fetchFromGitHub {
            owner = "dhall-lang";

            repo = "dhall-haskell";

            inherit (json) rev sha256 fetchSubmodules;
          };

      in
        import "${dhall-haskell}/default.nix";

    inherit (pkgsNew.dhall-haskell-derivations) dhall dhall-try;

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

    pythonPackages = pkgsOld.pythonPackages.override (old: {
        overrides =
          let
            extension = pythonPackagesNew: pythonPackagesOld: {
              recommonmark = pythonPackagesOld.recommonmark.overrideAttrs (old: {
                  patches =
                    (old.patches or []) ++ [
                      (pkgsNew.fetchpatch {
                          url = "https://patch-diff.githubusercontent.com/raw/readthedocs/recommonmark/pull/181.patch";

                          sha256 = "0rp6qi95cml1g1mplfkmc7lpv7czmg5rs1zqjkf5b4d3mkzm6bjs";
                        }
                      )
                    ];
                }
              );
            };

          in
            pkgsNew.lib.composeExtensions
              (old.overrides or (_: _: {}))
              extension;
      }
    );

    expected-test-files =
      pkgsNew.runCommand "expected-test-files" {} ''
        ${pkgsNew.rsync}/bin/rsync --archive ${./tests}/ "$out"

        ${pkgsNew.coreutils}/bin/chmod --recursive u+w "$out"

        for FILE in $(${pkgsNew.findutils}/bin/find "$out" -type f -name '*.dhallb'); do
          ${pkgsNew.cbor-diag}/bin/cbor2diag.rb "$FILE" > "''${FILE%.dhallb}.diag"
        done

        ${pkgsNew.dhall}/bin/dhall type --file "${./.}/tests/type-inference/success/preludeA.dhall" > "$out/type-inference/success/preludeB.dhall"
      '';

    expected-prelude = pkgsNew.runCommand "expected-prelude" {} ''
      ${pkgsNew.rsync}/bin/rsync --archive ${./Prelude}/ "$out"

      ${pkgsNew.coreutils}/bin/chmod --recursive u+w "$out"

      for FILE in $(${pkgsNew.findutils}/bin/find "$out" -type f ! -name README.md); do
        ${pkgsNew.dhall}/bin/dhall lint --inplace "$FILE"
        XDG_CACHE_HOME=/var/empty ${pkgsNew.dhall}/bin/dhall freeze --all --cache --inplace "$FILE"
      done
    '';

    test-files-lint = pkgsNew.runCommand "test-files-lint" {} ''
      ${pkgsNew.rsync}/bin/rsync --archive ${pkgsNew.expected-test-files}/ ./tests.expected
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

    docs =
      pkgsNew.runCommand "docs"
        { nativeBuildInputs = [
            pkgsNew.pythonPackages.sphinx
            pkgsNew.pythonPackages.sphinx_rtd_theme
            pkgsNew.pythonPackages.recommonmark
            pkgsNew.pythonPackages.pygments
          ];
        }
        ''
        SOURCE_DATE_EPOCH="$(${pkgsNew.coreutils}/bin/date '+%s')"

        sphinx-build ${./docs} $out

        cp ${./img/dhall-logo.svg} $out/_static/dhall-logo.svg
        '';

    logo = {
      argocd =
        pkgsNew.fetchurl {
          url    = "https://raw.githubusercontent.com/argoproj/argo-cd/master/docs/assets/argo.png";
          sha256 = "0gvfd7y7ihqyz93by730w0f6kdfs8dlvxv45ydccih94rxj3j7ac";
        };

      bash =
        pkgsNew.fetchurl {
          url    = "https://raw.githubusercontent.com/odb/official-bash-logo/master/assets/Logos/Icons/PNG/128x128.png";
          sha256 = "0fybbp6hbqrfw80fbk55bnykzda0m7x4vk38i80bjlmfbrkfvild";
        };

      clojure =
        pkgsNew.fetchurl {
          url    = "https://upload.wikimedia.org/wikipedia/commons/5/5d/Clojure_logo.svg";
          sha256 = "0mrjzv690g9mxljzxsvay8asyr8vlxhhs9smmax7mp3psd49b43g";
        };

      golang =
        pkgsNew.fetchzip {
          # See also https://blog.golang.org/go-brand
          url    = "https://storage.googleapis.com/golang-assets/go-logos-v1.0.zip";
          sha256 = "06vlpk22nxl1a3mz9rpk9fyq483rc4ml8f7lkkfqmabbia9ah7np";
          stripRoot = false;
        };

      ruby =
        pkgsNew.fetchurl {
          url    = "https://upload.wikimedia.org/wikipedia/commons/7/73/Ruby_logo.svg";
          sha256 = "1yvvdqcmgpa75y7px3isi4x6690iksq52ilnbslhn7mcngikw6m9";
        };

      rust =
        pkgsNew.fetchurl {
          url    = "http://rust-lang.org/logos/rust-logo-128x128-blk.png";
          sha256 = "19ycf7ra6pn6gvavpfg1gbi9j8dsmxfm0gnczabvpspv7yaf8i71";
        };

      dhallLarge =
        pkgsNew.fetchurl {
          url    = "https://raw.githubusercontent.com/dhall-lang/dhall-lang/28f4fb830f158bba6bb635bd29f1fd7075501b8f/img/dhall-logo.svg";
          sha256 = "19hqz6ipyb7fw460gnz9wkjlzllw1hpls9kahis12p9xr7a6rfb1";
        };

      dhallSmall =
        pkgsNew.fetchurl {
          url    = "https://raw.githubusercontent.com/dhall-lang/dhall-lang/28f4fb830f158bba6bb635bd29f1fd7075501b8f/img/dhall-icon.svg";
          sha256 = "0cxwvvhay54fq908ncyfmvq6jyjrbq565g4vrzk97z7z4qv3h5hh";
        };

      discourse = ./nixops/discourse.svg;

      github =
        pkgsNew.fetchzip {
          url    = "https://github-media-downloads.s3.amazonaws.com/GitHub-Mark.zip";
          sha256 = "0qy901f9rjzi0dyd1rb7zas50lcxy42sc7wnxpz2j2hr9ckg2zlz";
          stripRoot = false;
        };

      haskell =
        pkgsNew.fetchurl {
          url    = "https://wiki.haskell.org/wikiupload/4/4a/HaskellLogoStyPreview-1.png";
          sha256 = "0g26j7vx34m46mwp93qgg3q5x8pfdq2j1ch0vxz5gj0nk3b8fxda";
        };

      json =
        pkgsNew.fetchurl {
          url    = "https://upload.wikimedia.org/wikipedia/commons/c/c9/JSON_vector_logo.svg";
          sha256 = "1hqd1qh35v9magjp3rbsw8wszk2wn3hkz981ir49z5cyf11jnx95";
        };

      kops =
        pkgsNew.fetchurl {
          url    = "https://raw.githubusercontent.com/kubernetes/kops/master/docs/img/logo-notext.svg";
          sha256 = "0gdi0pcrmvmb23dy8zp7z1z980cmj5aqpp9yxrsyp4dsj7flay8r";
        };

      kubernetes =
        pkgsNew.fetchurl {
          url    = "https://raw.githubusercontent.com/kubernetes/kubernetes/7839fe38620508eb0651930cb0e1acb8ea367842/logo/logo.svg";
          sha256 = "0kp6idffg9k52ycgv5zkg9n08pfldzsy0fzhwsrb2f7cvrl6fpw4";
        };

      nix =
        pkgsNew.fetchurl {
          url    = "https://nixos.org/logo/nix-wiki.png";
          sha256 = "1hrz7wr7i0b2bips60ygacbkmdzv466lsbxi22hycg42kv4m0173";
        };

      openCollective =
        pkgsNew.fetchurl {
          url    = "https://opencollective.com/static/images/opencollective-icon.svg";
          sha256 = "0i4hngjycmrj6gk2knsxqy35sx1ksmc268lr56s1fwfdp6afsh4z";
        };

      prometheus =
        pkgsNew.fetchurl {
          url    = "https://upload.wikimedia.org/wikipedia/commons/3/38/Prometheus_software_logo.svg";
          sha256 = "19ff8l1kp3i3gxxbd5na9wbzxkpflcxw0lz2ysb1d6s4ybvr0fnb";
        };

      stackOverflow =
        pkgsNew.fetchurl {
          url    = "https://cdn.sstatic.net/Sites/stackoverflow/company/img/logos/so/so-icon.svg";
          sha256 = "0i84h23ax197f3hwh0hqm6yjvvnpcjyhd6nkyy33z6x10dh8v4z3";
        };

      twitter = pkgsNew.callPackage ./nixops/twitterLogo.nix { };

      xml =
        pkgsNew.fetchurl {
          url    = "https://www.svgrepo.com/download/106090/xml.svg";
          sha256 = "0ncjr3sxn40ml5gxwv1iq5vd0zx0qgyy9rzqxi6j80b8wcy45czy";
        };

      yaml =
        pkgsNew.fetchurl {
          url    = "https://raw.githubusercontent.com/yaml/yaml-spec/a6f764e13de58d5f753877f588a01b35dc9a5168/logo.png";
          sha256 = "12grgaxpqi755p2rnvw3x02zc69brpnzx208id1f0z42w387j4hi";
        };
    };

    npm = pkgsNew.callPackage ./nixops/npm { };

    jQuery =
      pkgsNew.fetchurl {
        url    = "https://code.jquery.com/jquery-3.3.1.min.js";
        sha256 = "1vq2bp290rhby5l09dv5khqwv3ysnzbddggbgk6m4hl9y9pl42hn";
      };

    twitterBootstrap = pkgsNew.callPackage ./nixops/twitterBootstrap.nix { };

    website = pkgsNew.callPackage ./nixops/website.nix {};

    tarball-website = pkgsNew.releaseTools.binaryTarball rec {
      src = pkgsNew.website;

      installPhase = ''
        releaseName=website
        ${pkgsNew.coreutils}/bin/install --target-directory "$TMPDIR/inst/website/"    -D $src/index.html
        ${pkgsNew.coreutils}/bin/install --target-directory "$TMPDIR/inst/website/img" -D $src/img/*
        ${pkgsNew.coreutils}/bin/install --target-directory "$TMPDIR/inst/website/css" -D $src/css/*
        ${pkgsNew.coreutils}/bin/install --target-directory "$TMPDIR/inst/website/js"  -D $src/js/*
      '';
    };
  };

  pkgs = import nixpkgs { config = {}; overlays = [ overlay ]; };

  # Derivation that trivially depends on the input source code revision.
  # As this is included in the "dhall-lang" aggregate, it forces every
  # commit to have a corresponding GitHub status check, even if the
  # commit doesn't make any changes (which can happen when merging
  # master in).
  rev = pkgs.runCommand "rev" {} ''echo "${src.rev}" > $out'';

  machine =
    (import "${dhallLangNixpkgs}/nixos" {
      configuration = {
        imports = [ ./nixops/logical.nix ./nixops/physical.nix ];

        networking.hostName = "dhall-lang";

        nixpkgs.overlays = [ overlay ];
      };

      system = "x86_64-linux";
    }).system;

in
  { dhall-lang = pkgs.releaseTools.aggregate {
      name = "dhall-lang";

      constituents = [
        pkgs.dhall-grammar
        pkgs.ensure-trailing-newlines
        pkgs.prelude-lint
        pkgs.test-files-lint
        machine
        rev
      ];
    };

    inherit (pkgs) expected-prelude expected-test-files docs website;

    inherit machine;
  }
