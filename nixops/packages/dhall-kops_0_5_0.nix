{ buildDhallGitHubPackage, Prelude }:
  buildDhallGitHubPackage {
    name = "dhall-kops";
    githubBase = "github.com";
    owner = "coralogix";
    repo = "dhall-kops";
    rev = "v0.5.0";
    fetchSubmodules = false;
    sha256 = "18i4aq47izwwcxi979747y8pyx9pzyfglwkhzn4a8nafx2rkvfm3";
    directory = "";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [ (Prelude.overridePackage { file = "package.dhall"; }) ];
    }
