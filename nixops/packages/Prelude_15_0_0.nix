{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "Prelude";
    githubBase = "github.com";
    owner = "dhall-lang";
    repo = "dhall-lang";
    rev = "v15.0.0";
    fetchSubmodules = false;
    sha256 = "0kkl7qzpc99gpskcr4f471xdvig2bynay8f6i90ws3224rvxvf3r";
    directory = "Prelude";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
