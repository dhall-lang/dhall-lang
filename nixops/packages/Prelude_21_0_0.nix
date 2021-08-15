{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "Prelude";
    githubBase = "github.com";
    owner = "dhall-lang";
    repo = "dhall-lang";
    rev = "v21.0.0";
    fetchSubmodules = false;
    sha256 = "1v7p0vfr5dlbs4xak4699azv6b14qzfnbm8j8n5idv698zybmyis";
    directory = "Prelude";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
