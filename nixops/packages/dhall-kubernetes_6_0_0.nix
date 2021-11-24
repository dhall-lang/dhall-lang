{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "dhall-kubernetes";
    githubBase = "github.com";
    owner = "dhall-lang";
    repo = "dhall-kubernetes";
    rev = "v6.0.0";
    fetchSubmodules = false;
    sha256 = "1jj4iz4bzaf7jcb85h8anh3iwlnrv16a23z3s9i8zxfr20y097m7";
    directory = "";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
