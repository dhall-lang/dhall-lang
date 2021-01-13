{ buildDhallGitHubPackage, Prelude }:
  buildDhallGitHubPackage {
    name = "dhall-kops";
    githubBase = "github.com";
    owner = "coralogix";
    repo = "dhall-kops";
    rev = "v0.5.1";
    fetchSubmodules = false;
    sha256 = "1qsk2pvylcn94cdq3zcqz74mvs3ijnawj411bz4xdikyfp0n5iw4";
    directory = "";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [ (Prelude.overridePackage { file = "package.dhall"; }) ];
    }
