{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "dhall-concourse";
    githubBase = "github.com";
    owner = "coralogix";
    repo = "dhall-concourse";
    rev = "v0.8.0";
    fetchSubmodules = false;
    sha256 = "0pncqcm4fp9s0ms1ir0dxd1x434mqf1wg71m1wfjq6lbaa50gx5h";
    directory = "";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
