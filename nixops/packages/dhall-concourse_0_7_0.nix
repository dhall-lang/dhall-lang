{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "dhall-concourse";
    githubBase = "github.com";
    owner = "coralogix";
    repo = "dhall-concourse";
    rev = "v0.7.0";
    fetchSubmodules = false;
    sha256 = "1wvhb8h2qzn6i1n6lra4iv482w1f43yznq7aybvip58ak3dgi716";
    directory = "";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
