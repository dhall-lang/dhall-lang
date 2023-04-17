{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "Prelude";
    githubBase = "github.com";
    owner = "dhall-lang";
    repo = "dhall-lang";
    rev = "v23.0.0";
    fetchSubmodules = false;
    sha256 = "00dkysbyhmrbsflqz4lddsqmx7ns0gg8i5j86vadbrz14w33jih0";
    directory = "Prelude";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
