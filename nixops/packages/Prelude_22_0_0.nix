{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "Prelude";
    githubBase = "github.com";
    owner = "dhall-lang";
    repo = "dhall-lang";
    rev = "v22.0.0";
    fetchSubmodules = false;
    sha256 = "157x95yw9hibpifa8figawgclnc1d1bapmbp1zw60l2viy936b83";
    directory = "Prelude";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
