{ buildDhallGitHubPackage, Prelude }:
  buildDhallGitHubPackage {
    name = "dhall-dot";
    githubBase = "github.com";
    owner = "Gabriel439";
    repo = "dhall-dot";
    rev = "v1.0.0";
    fetchSubmodules = false;
    sha256 = "0yawb4icvff8yqk8hg2czj7ld73gqij6zwdg3gjshwqwy5afzj9k";
    directory = "";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [
      (Prelude.overridePackage { file = "List/map.dhall"; })
      (Prelude.overridePackage { file = "Text/concatSep.dhall"; })
      (Prelude.overridePackage { file = "Text/concatMapSep.dhall"; })
      (Prelude.overridePackage { file = "List/null.dhall"; })
      ];
    }
