{ buildDhallGitHubPackage, directory ? "" }:
  buildDhallGitHubPackage {
    name = "dhall-kubernetes-${directory}";
    githubBase = "github.com";
    owner = "dhall-lang";
    repo = "dhall-kubernetes";
    rev = "v5.0.0";
    fetchSubmodules = false;
    sha256 = "0irqv44nh6fp3nyal48rzp5ir0y82r897aaw2nnc4yrfh9rd8w0y";
    inherit directory;
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
