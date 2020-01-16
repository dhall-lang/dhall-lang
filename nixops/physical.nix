let
  region = "us-west-1";

in
  { ipfs = { resources, ... }: {
      deployment = {
        targetEnv = "ec2";

        ec2 = {
          inherit region;

          inherit (resources.ec2KeyPairs) keyPair;

          instanceType = "t2.micro";
        };
      };
    };

    hydra = { ... }: {
      imports = [ ./hydra-physical.nix ];

      deployment = {
        targetEnv = "none";

        targetHost = "hydra.dhall-lang.org";
      };
    };

    resources.ec2KeyPairs.keyPair = { inherit region; };
  }
