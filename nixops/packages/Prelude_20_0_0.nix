{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "dhall-lang";
    githubBase = "github.com";
    owner = "dhall-lang";
    repo = "dhall-lang";
    rev = "v20.0.0";
    fetchSubmodules = false;
    sha256 = "1smk57xki1cj24xpp0s3gv85radl6ry76ybsjkqak8h13s79lwla";
    directory = "Prelude";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
