{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "Prelude";
    githubBase = "github.com";
    owner = "dhall-lang";
    repo = "dhall-lang";
    rev = "v20.1.0";
    fetchSubmodules = false;
    sha256 = "04r1w7wqydmwm9mh3lz4y96a87k5kkvzsmrhbdrf0izcy5bqqv5y";
    directory = "Prelude";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
