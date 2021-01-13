{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "dhall-concourse";
    githubBase = "github.com";
    owner = "coralogix";
    repo = "dhall-concourse";
    rev = "v0.9.1";
    fetchSubmodules = false;
    sha256 = "0psvmwzxx81njrc8nkjl8a0iz2jxyfahqb8cr1im81z73angs79z";
    directory = "";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
