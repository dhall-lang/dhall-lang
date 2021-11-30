{ buildDhallGitHubPackage, callPackage }:
callPackage ./dhall-kubernetes_6_0_0.nix { directory = "1.21"; }
