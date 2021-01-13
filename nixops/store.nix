{ dhallPackages, runCommand, xorg }:

let
  packages = with dhallPackages; [
    dhall-ansible_0_1_0

    dhall-concourse_0_4_1
    dhall-concourse_0_5_0
    dhall-concourse_0_5_1
    dhall-concourse_0_5_2
    dhall-concourse_0_6_0
    dhall-concourse_0_6_1
    dhall-concourse_0_7_0
    dhall-concourse_0_7_1
    dhall-concourse_0_8_0
    dhall-concourse_0_9_0
    dhall-concourse_0_9_1

    dhall-dot_1_0_0

    dhall-kubernetes_3_0_0
    dhall-kubernetes_4_0_0
    dhall-kubernetes_5_0_0

    (dhall-semver_1_0_0.override {
      Prelude = Prelude_17_0_0;
    })

    Prelude_14_0_0
    Prelude_15_0_0
    Prelude_16_0_0
    Prelude_17_0_0
    Prelude_18_0_0
    Prelude_19_0_0
    Prelude_20_0_0
  ];

in
  runCommand "store" { inherit packages; } ''
    mkdir "$out"

    for package in $packages; do
      ${xorg.lndir}/bin/lndir -silent "$package/.cache/dhall" "$out"
    done
  ''
