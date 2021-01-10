{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "dhall-concourse";
    githubBase = "github.com";
    owner = "coralogix";
    repo = "dhall-concourse";
    rev = "v0.6.0";
    fetchSubmodules = false;
    sha256 = "03mcbifjgp1564wzjp40pqrb3vcni4v8jdxdpf9dgdynh70hqfq4";
    directory = "";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
