{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "dhall-lang";
    githubBase = "github.com";
    owner = "dhall-lang";
    repo = "dhall-lang";
    rev = "abf9f8a516dfd69522e6240fc43e6f139f7f2e71";
    fetchSubmodules = false;
    sha256 = "0rj7agshczwr2dw72x3j8x3cywabyw51v1wy7dd8j0s7gv9kvank";
    directory = "Prelude";
    file = "package.dhall";
    source = false;
    document = true;
    dependencies = [];
    }
