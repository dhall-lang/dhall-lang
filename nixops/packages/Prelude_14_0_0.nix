{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "Prelude";
    githubBase = "github.com";
    owner = "dhall-lang";
    repo = "dhall-lang";
    rev = "v14.0.0";
    fetchSubmodules = false;
    sha256 = "1b4h6zk8b6yylgl3g4dvfqdkvabnaxxa9flaw1py20p77nxnfyfq";
    directory = "Prelude";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
