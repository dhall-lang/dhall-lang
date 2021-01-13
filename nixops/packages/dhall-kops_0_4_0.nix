{ buildDhallGitHubPackage, Prelude }:
  buildDhallGitHubPackage {
    name = "dhall-kops";
    githubBase = "github.com";
    owner = "coralogix";
    repo = "dhall-kops";
    rev = "v0.4.0";
    fetchSubmodules = false;
    sha256 = "0avy6q1b84qgi43ikrz78hfpslg80mkkadsdij4a425q8pc9v0a8";
    directory = "";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [ (Prelude.overridePackage { file = "package.dhall"; }) ];
    }
