{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "dhall-kubernetes";
    githubBase = "github.com";
    owner = "dhall-lang";
    repo = "dhall-kubernetes";
    rev = "v4.0.0";
    fetchSubmodules = false;
    sha256 = "0j5d9rdiwa0zajmqak1ddlmbq2lrkcsc89aa70bhlkq33d1adwyh";
    directory = "";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
