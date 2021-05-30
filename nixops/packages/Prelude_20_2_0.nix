{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "Prelude";
    githubBase = "github.com";
    owner = "dhall-lang";
    repo = "dhall-lang";
    rev = "v20.2.0";
    fetchSubmodules = false;
    sha256 = "0wqcj5w4x24j3p73qrc5x5gci2mxkvli3kphg53qwis4b8g703zp";
    directory = "Prelude";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
