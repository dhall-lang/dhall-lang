{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "dhall-concourse";
    githubBase = "github.com";
    owner = "coralogix";
    repo = "dhall-concourse";
    rev = "v0.5.1";
    fetchSubmodules = false;
    sha256 = "180jif6k7y0q6xfyj8psi39zw9i7rdp4vdjzn3hxhaw9j93ghkfw";
    directory = "";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
