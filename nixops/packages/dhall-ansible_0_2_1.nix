{ buildDhallGitHubPackage, Prelude }:
  buildDhallGitHubPackage {
    name = "dhall-ansible";
    githubBase = "github.com";
    owner = "softwarefactory-project";
    repo = "dhall-ansible";
    rev = "0.2.1";
    fetchSubmodules = false;
    sha256 = "0xckr09alcn914ax3aa8nvbwcsg6fv3vp5l7hrfrxjv8fp8glv3s";
    directory = "";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [
      (Prelude.overridePackage { file = "JSON/Type"; })
      (Prelude.overridePackage { file = "JSON/core.dhall"; })
      (Prelude.overridePackage { file = "Map/map"; })
      ];
    }
