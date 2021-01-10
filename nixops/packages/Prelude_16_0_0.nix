{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "dhall-lang";
    githubBase = "github.com";
    owner = "dhall-lang";
    repo = "dhall-lang";
    rev = "v16.0.0";
    fetchSubmodules = false;
    sha256 = "1lnpvrhxa5fh2721biw2nd69qwiqvclw6z6ywiv9xvpmw2alwdsb";
    directory = "Prelude";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
