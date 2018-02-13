{ pullRequestsJSON, nixpkgs, suffix, ... }:

let
  pkgs = import nixpkgs { config = {}; };

  pullRequests = builtins.fromJSON (builtins.readFile pullRequestsJSON);

  toJobset = num: info: {
    enabled = 1;

    hidden = false;

    description = info.title;

    nixexprinput = "src";

    nixexprpath = "release.nix";

    checkinterval = 20;

    schedulingshares = 1;

    enableemail = false;

    emailoverride = "";

    keepnr = 1;

    inputs = {
      src = {
        type = "git";

        value = "https://github.com/${info.base.repo.owner.login}/${info.base.repo.name}.git ${info.head.sha}";

        emailresponsible = false;
      };

      nixpkgs = {
        type = "git";

        value = "https://github.com/NixOS/nixpkgs.git release-17.09";

        emailresponsible = false;
      };
    };
  };

  master = toJobset "master" {
    base.repo = { owner.login = "dhall-lang"; name = "dhall-${suffix}"; };

    head.sha = "master";

    title = "master";
  };

  jobsets = pkgs.lib.mapAttrs toJobset pullRequests // { inherit master; };

in
  { jobsets = pkgs.writeText "jobsets.json" (builtins.toJSON jobsets);
  }
