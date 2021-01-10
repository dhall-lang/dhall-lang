{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "dhall-ansible";
    githubBase = "github.com";
    owner = "softwarefactory-project";
    repo = "dhall-ansible";
    rev = "0.1.0";
    fetchSubmodules = false;
    sha256 = "1s7aphfhbbcnmisg5aqqc6ssqyd8gw3vxicynlk9c78nxnwzc539";
    directory = "";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
