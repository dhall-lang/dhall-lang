{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "dhall-concourse";
    githubBase = "github.com";
    owner = "coralogix";
    repo = "dhall-concourse";
    rev = "v0.9.0";
    fetchSubmodules = false;
    sha256 = "14jc35iqy38p2n5j32rqh9lhfqss8lj7akp3xn70bjf7kz635vrs";
    directory = "";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
