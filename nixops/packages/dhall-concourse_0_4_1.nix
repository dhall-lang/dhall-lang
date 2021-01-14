{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "dhall-concourse";
    githubBase = "github.com";
    owner = "coralogix";
    repo = "dhall-concourse";
    rev = "v0.4.1";
    fetchSubmodules = false;
    sha256 = "0hq43z0a6f4jvfap6wbb7i871sii05wasfr4sgm4ldb0arn6sljp";
    directory = "";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
