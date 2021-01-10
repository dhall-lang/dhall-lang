{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "dhall-concourse";
    githubBase = "github.com";
    owner = "coralogix";
    repo = "dhall-concourse";
    rev = "v0.5.0";
    fetchSubmodules = false;
    sha256 = "16g1bh261nsdy166sbr87d7x931mi73xynwsf7m686g62klmsdrz";
    directory = "";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
