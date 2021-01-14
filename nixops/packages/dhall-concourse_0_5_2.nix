{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "dhall-concourse";
    githubBase = "github.com";
    owner = "coralogix";
    repo = "dhall-concourse";
    rev = "v0.5.2";
    fetchSubmodules = false;
    sha256 = "1dr447w4a914i770a0h0366lbccxkj17si516cikqrhvln1fqmqs";
    directory = "";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
