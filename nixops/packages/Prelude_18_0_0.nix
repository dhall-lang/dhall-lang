{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "Prelude";
    githubBase = "github.com";
    owner = "dhall-lang";
    repo = "dhall-lang";
    rev = "v18.0.0";
    fetchSubmodules = false;
    sha256 = "1vx3cdzpdrbjjc214v6mnl9y2k4yrpy3fgj37fg2dlirbqppdk9r";
    directory = "Prelude";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
