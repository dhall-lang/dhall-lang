{ coreutils, dhallPackages, lib, runCommand, writeText, xorg }:

let
  packages = with dhallPackages; [
    dhall-ansible_0_1_0
    # A semantic integrity check fails for newer versions of Dhall
    # dhall-ansible_0_2_1

    # This is currently too expensive for CI to build
    # TODO: fix performance issues
    # dhall-aws-cloudformation_0_4_21

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
    dhall-kubernetes_v6_0_0-1_19
    dhall-kubernetes_v6_0_0-1_20
    dhall-kubernetes_v6_0_0-1_21

    (dhall-nomad_1_0_0.override {
      Prelude = Prelude_20_0_0;
    })

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
    Prelude_20_1_0
    Prelude_20_2_0
    Prelude_21_0_0
    Prelude_21_1_0
  ];

  toListItem =
    package:
      ''
      <li><a href="./${package.name}">${package.name}</a></li>
      '';

  index = writeText "index.html"
    ''
    <!DOCTYPE HTML>
    <html>
      <head>
        <title>Dhall packages</title>
        <link href="index.css" type="text/css" rel="stylesheet">
        <link href="https://fonts.googleapis.com/css2?family=Fira+Code:wght@400;500;600;700&amp;family=Lato:ital,wght@0,400;0,700;1,400&amp;display=swap" type="text/css" rel="stylesheet">
        <script src="index.js" type="text/javascript"></script>
        <meta charset="UTF-8">
      </head>
      <body>
        <div class="nav-bar">
          <img src="dhall-icon.svg" class="dhall-icon">
          <p class="package-title">Dhall packages</p>
          <div class="nav-bar-content-divider"></div>
          <a id="switch-light-dark-mode" class="nav-option">Switch Light/Dark Mode</a>
        </div>
        <div class="main-container">
          <h2>Package index</h2>
          <ul>
          ${lib.concatMapStrings toListItem packages}
          </ul>
        </div>
      </body>
    </html>
    '';
in
  runCommand "store" { inherit packages; } ''
    ${coreutils}/bin/mkdir "$out"

    ${coreutils}/bin/cp '${index}' "$out/index.html"
    ${coreutils}/bin/ln --symbolic '${./index.css}' "$out/index.css"
    ${coreutils}/bin/ln --symbolic '${./index.js}' "$out/index.js"
    ${coreutils}/bin/ln --symbolic '${./dhall-icon.svg}' "$out/dhall-icon.svg"

    for package in $packages; do
      ${xorg.lndir}/bin/lndir -silent "$package/.cache/dhall" "$out"
      ${coreutils}/bin/ln --symbolic "$package/docs" "$out/''${package:44}"
    done
  ''
