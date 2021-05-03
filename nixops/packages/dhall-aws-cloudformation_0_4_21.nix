{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "dhall-aws-cloudformation";
    githubBase = "github.com";
    owner = "jcouyang";
    repo = "dhall-aws-cloudformation";
    rev = "0.4.21";
    fetchSubmodules = false;
    sha256 = "0jksaqvny2a7l0l3xcsi7gpx53krfv9lmd8n6bsmqwbh17xsa3rw";
    directory = "";
    file = "version.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
