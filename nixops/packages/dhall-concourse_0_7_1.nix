{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "dhall-concourse";
    githubBase = "github.com";
    owner = "coralogix";
    repo = "dhall-concourse";
    rev = "v0.7.1";
    fetchSubmodules = false;
    sha256 = "1fc38ax58w9mpnwnhdd0h6cid0i5n6p3156k9zxb42bs889rgm6q";
    directory = "";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
