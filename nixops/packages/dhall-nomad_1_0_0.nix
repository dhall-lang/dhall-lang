{ buildDhallGitHubPackage, Prelude }:
  buildDhallGitHubPackage {
    name = "dhall-nomad";
    githubBase = "github.com";
    owner = "seatgeek";
    repo = "dhall-nomad";
    rev = "v1.0.0";
    fetchSubmodules = false;
    sha256 = "0ykbj6gwghdh1g2w3k7xl1v1cfj3xpvq3ac52nlwiyi6cc6dvvjf";
    directory = "";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [ (Prelude.overridePackage { file = "package.dhall"; }) ];
    }
