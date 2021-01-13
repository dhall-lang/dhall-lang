{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "dhall-lang";
    githubBase = "github.com";
    owner = "dhall-lang";
    repo = "dhall-lang";
    rev = "v19.0.0";
    fetchSubmodules = false;
    sha256 = "04m29f5xlks6rarv1gy909j68bsflwl18l9bg7kyy1vpwap0avkp";
    directory = "Prelude";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
