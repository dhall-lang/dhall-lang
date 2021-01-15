{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "Prelude";
    githubBase = "github.com";
    owner = "dhall-lang";
    repo = "dhall-lang";
    rev = "v17.0.0";
    fetchSubmodules = false;
    sha256 = "0jnqw50q26ksxkzs85a2svyhwd2cy858xhncq945bmirpqrhklwf";
    directory = "Prelude";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
