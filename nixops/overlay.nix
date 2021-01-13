pkgsNew: pkgsOld: {
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

      sphinx-build ${../docs} $out

      cp ${../img/dhall-logo.svg} $out/_static/dhall-logo.svg
      '';

  dhall =
    pkgsNew.haskell.lib.dontCheck pkgsNew.dhall-haskell-derivations.dhall;

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

  dhall-haskell-derivations =
    let
      json = builtins.fromJSON (builtins.readFile ./dhall-haskell.json);

      dhall-haskell =
        pkgsNew.fetchFromGitHub {
          owner = "dhall-lang";

          repo = "dhall-haskell";

          inherit (json) rev sha256 fetchSubmodules;
        };

    in
      import "${dhall-haskell}/default.nix";

  inherit (pkgsNew.dhall-haskell-derivations) dhall-try;

  dhallPackages = pkgsOld.dhallPackages.override (old: {
      overrides =
        let
          directoryOverrides = dhallPackagesNew: dhallPackagesOld:
            let
              files = builtins.readDir ./packages;

              toPackage = file: _ :
                { name =
                    builtins.replaceStrings [ ".nix" ] [ "" ] file;

                  value =
                    dhallPackagesNew.callPackage
                      (./packages + "/${file}")
                      { };
                };

            in
              pkgsNew.lib.mapAttrs' toPackage files;

        in
          pkgsNew.lib.composeExtensions
            (old.overrides or (_: _: {}))
            directoryOverrides;
    }
  );

  ensure-trailing-newlines = pkgsNew.runCommand "ensure-trailing-newlines" {} ''
    for FILE in $(${pkgsNew.findutils}/bin/find ${../tests} -type f -name '*.dhall'); do
      LAST_CHARACTER=$(${pkgsNew.coreutils}/bin/tail --bytes 1 "$FILE")

      if [ ! -s "''${FILE}" -o "$LAST_CHARACTER" != "" ]; then
        echo "''${FILE} is missing a trailing newline"

        exit 1
      fi
    done

    touch $out
  '';

  expected-prelude = pkgsNew.runCommand "expected-prelude" {} ''
    ${pkgsNew.rsync}/bin/rsync --archive ${../Prelude}/ "$out"

    ${pkgsNew.coreutils}/bin/chmod --recursive u+w "$out"

    for FILE in $(${pkgsNew.findutils}/bin/find "$out" -type f ! -name README.md); do
      ${pkgsNew.dhall}/bin/dhall lint --inplace "$FILE"
      XDG_CACHE_HOME=/var/empty ${pkgsNew.dhall}/bin/dhall freeze --all --cache --inplace "$FILE"
    done
  '';

  expected-test-files =
    pkgsNew.runCommand "expected-test-files" {} ''
      ${pkgsNew.rsync}/bin/rsync --archive ${../tests}/ "$out"

      ${pkgsNew.coreutils}/bin/chmod --recursive u+w "$out"

      for FILE in $(${pkgsNew.findutils}/bin/find "$out" -type f -name '*.dhallb'); do
        ${pkgsNew.cbor-diag}/bin/cbor2diag.rb "$FILE" > "''${FILE%.dhallb}.diag"
      done

      ${pkgsNew.dhall}/bin/dhall type --file "${../.}/tests/type-inference/success/preludeA.dhall" > "$out/type-inference/success/preludeB.dhall"
    '';

  hydra-unstable = pkgsOld.hydra-unstable.overrideAttrs (old: {
      patches = (old.patches or []) ++ [
        ./hydra.patch
        ./no-restrict-eval.patch
        (pkgsNew.fetchpatch {
            url = "https://github.com/NixOS/hydra/commit/df3262e96cb55bdfaac7726896728bfef675698b.patch";

            sha256 = "1344cqlmx0ncgsh3dqn5igbxx6rgmlm14rgb5vi6rxkvwnfqy3zj";
          }
        )
      ];
    }
  );

  instaparse-check = pkgsNew.writeText "build.boot" ''
    (require '[instaparse.core :refer [parser]])

    (def grammar (slurp "${../standard/dhall.abnf}"))

    (def dhall-parser
      (parser grammar
              :input-format :abnf
              :start :complete-expression
              :output-format :hiccup))

    (deftask check []
      (println "Grammar is syntactically correct."))
  '';

  jQuery =
    pkgsNew.fetchurl {
      url    = "https://code.jquery.com/jquery-3.3.1.min.js";
      sha256 = "1vq2bp290rhby5l09dv5khqwv3ysnzbddggbgk6m4hl9y9pl42hn";
    };

  logo = {
    ansible =
      pkgsNew.fetchurl {
        url    = "https://www.ansible.com/hubfs/2016_Images/Assets/Ansible-Mark-Large-RGB-Mango.png";
        sha256 = "1zmd6gnx6gx9z6n5i02ipidc2ypakhhv07nznr3a5jjbyl4qqj3y";
      };

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

    concourse =
      pkgsNew.fetchurl {
        url    = "https://raw.githubusercontent.com/concourse/brand/c26ab042b2d241ccdc748f9046539b363a5a13cd/concourse_logo/concourse_logo_b.png";
        sha256 = "1y02cfz1jhjmmlqa26ifnnivcaa8f6m217n3qf57drgbhrwndnqv";
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

    discourse = ./discourse.svg;

    docker =
      pkgsNew.fetchurl {
        url    = "https://www.docker.com/sites/default/files/d8/2019-07/Moby-logo.png";
        sha256 = "0k8vaai7xdqq19b3c52i2m48v8i4bm8307i46d8vkwwz4fp6f7fi";
      };

    github =
      pkgsNew.fetchzip {
        url    = "https://github-media-downloads.s3.amazonaws.com/GitHub-Mark.zip";
        sha256 = "0qy901f9rjzi0dyd1rb7zas50lcxy42sc7wnxpz2j2hr9ckg2zlz";
        stripRoot = false;
      };

    gitlab =
      pkgsNew.fetchurl {
        url    = "https://about.gitlab.com/images/press/logo/png/gitlab-icon-rgb.png";
        sha256 = "1mxk6xxw8bzp3l4jx3jaka9n5a69jbnkw4kzpmy4hbhp7nc33z5w";
      };

    golang =
      pkgsNew.fetchzip {
        # See also https://blog.golang.org/go-brand
        url    = "https://storage.googleapis.com/golang-assets/go-logos-v1.0.zip";
        sha256 = "06vlpk22nxl1a3mz9rpk9fyq483rc4ml8f7lkkfqmabbia9ah7np";
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

    slack =
      pkgsNew.fetchurl {
        name = "slack-logo.png";

        url =
          "https://brandfolder.com/slack/attachments/pl546j-7le8zk-afym5u?dl=true&resource_key=pl53se-o7edc-2zw45a&resource_type=Collection";

        sha256 = "0i4yjjgkcky6zfbim17rryy23pbrsc4255jzy14lgy7ja3a5jabk";

        postFetch =
          "${pkgsNew.imagemagickBig}/bin/mogrify -format png -crop 300x300+100+100 $downloadedFile";
      };

    stackOverflow =
      pkgsNew.fetchurl {
        url    = "https://cdn.sstatic.net/Sites/stackoverflow/company/img/logos/so/so-icon.svg";
        sha256 = "0i84h23ax197f3hwh0hqm6yjvvnpcjyhd6nkyy33z6x10dh8v4z3";
      };

    twitter = pkgsNew.callPackage ./twitterLogo.nix { };

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

    zulip =
      pkgsNew.fetchurl {
        url    = "https://raw.githubusercontent.com/zulip/zulip/28d58c848d60b2b93e82b2aa61c560268806ebb6/static/images/logo/zulip-icon-128x128.png";
        sha256 = "0j7khvym2p4f8dqdl7k7k4rypj1hai2rj0pjzb6h41fvjyjmbrrr";
      };
  };

  npm = pkgsNew.callPackage ./npm { };

  prelude-lint = pkgsNew.runCommand "prelude-lint" {} ''
    ${pkgsNew.rsync}/bin/rsync --archive ${pkgsNew.expected-prelude}/ ./Prelude.expected
    ${pkgsNew.rsync}/bin/rsync --archive ${../Prelude}/ ./Prelude.actual

    ${pkgsNew.diffutils}/bin/diff --recursive ./Prelude.{actual,expected}

    touch $out
  '';

  python27 = pkgsOld.python27.override (old: {
      packageOverrides =
        let
          extension = pythonPackagesNew: pythonPackagesOld: {
            cffi = pythonPackagesOld.cffi.overrideAttrs (old: {
                doInstallCheck = false;
              }
            );

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
            (old.packageOverrides or (_: _: {}))
            extension;
    }
  );

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

  test-files-lint = pkgsNew.runCommand "test-files-lint" {} ''
    ${pkgsNew.rsync}/bin/rsync --archive ${pkgsNew.expected-test-files}/ ./tests.expected
    ${pkgsNew.rsync}/bin/rsync --archive ${../tests}/ ./tests.actual

    ${pkgsNew.diffutils}/bin/diff --recursive ./tests.{actual,expected}

    touch $out
  '';

  twitterBootstrap = pkgsNew.callPackage ./twitterBootstrap.nix { };

  website = pkgsNew.callPackage ./website.nix {};
}
