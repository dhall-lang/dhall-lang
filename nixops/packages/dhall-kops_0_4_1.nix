{ buildDhallGitHubPackage, Prelude }:
  buildDhallGitHubPackage {
    name = "dhall-kops";
    githubBase = "github.com";
    owner = "coralogix";
    repo = "dhall-kops";
    rev = "v0.4.1";
    fetchSubmodules = false;
    sha256 = "10z0h4zbi65il4jl8lmc9amgqsxbnrj6jzz1ix1s972p0qm7mb0d";
    directory = "";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [ (Prelude.overridePackage { file = "package.dhall"; }) ];
    }
