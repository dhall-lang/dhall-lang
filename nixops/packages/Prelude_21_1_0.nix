{ buildDhallGitHubPackage }:
  buildDhallGitHubPackage {
    name = "Prelude";
    githubBase = "github.com";
    owner = "dhall-lang";
    repo = "dhall-lang";
    rev = "v21.1.0";
    fetchSubmodules = false;
    sha256 = "0ml2hv934lj3fl07mfkir6387z9sz5n62mm1k46sx5mbs6nhmcpd";
    directory = "Prelude";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [];
    }
