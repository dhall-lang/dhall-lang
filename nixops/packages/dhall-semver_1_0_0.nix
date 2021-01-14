{ buildDhallGitHubPackage, Prelude }:
  buildDhallGitHubPackage {
    name = "dhall-semver";
    githubBase = "github.com";
    owner = "Gabriel439";
    repo = "dhall-semver";
    rev = "v1.0.0";
    fetchSubmodules = false;
    sha256 = "0y8shvp8srzbjjpmnsvz9c12ciihnx1szs0yzyi9ashmrjvd0jcz";
    directory = "";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [ (Prelude.overridePackage { file = "package.dhall"; }) ];
    }
