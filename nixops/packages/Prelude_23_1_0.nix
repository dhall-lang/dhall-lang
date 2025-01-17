{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "Prelude";
    githubBase = "github.com";
    owner = "dhall-lang";
    repo = "dhall-lang";
    rev = "v23.1.0";
    fetchSubmodules = false;
    sha256 = "0vkx7cdj5lrgddra924b08j960fhw1r85lm7dq8hb8af18gincpg";
    directory = "Prelude";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
  }
