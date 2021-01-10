{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "dhall-concourse";
    githubBase = "github.com";
    owner = "coralogix";
    repo = "dhall-concourse";
    rev = "v0.6.1";
    fetchSubmodules = false;
    sha256 = "0ljyxzdxp16hhq6g8y6jhq4kxgichlcbiv91b016mwzvcakzj71f";
    directory = "";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
