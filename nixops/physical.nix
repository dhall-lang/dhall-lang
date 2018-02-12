let
  region = "us-west-1";

in
  { ipfs = { resources, ... }: {
      deployment = {
        targetEnv = "ec2";

        ec2 = {
          inherit region;

          inherit (resources.ec2KeyPairs) keyPair;

          instanceType = "t2.nano";
        };
      };
    };

    hydra = { ... }: {
      deployment = {
        targetEnv = "none";

        targetHost = "hydra.dhall-lang.org";
      };

      imports = [ <nixpkgs/nixos/modules/profiles/qemu-guest.nix> ];

      nixpkgs.system = "x86_64-linux";

      boot = {
        initrd.availableKernelModules =
          [ "ata_piix" "virtio_pci" "floppy" "sd_mod" ];

        kernelParams = [ "console=ttyS0,19200n8" ];

        loader = {
          grub = {
            enable = true;

            extraConfig = ''
              serial --speed=19200 --unit=0 --word=8 --parity=no --stop=1;
              terminal_input serial;
              terminal_output serial;
            '';

            device = "nodev";

            timeout = 10;

            version = 2;
          };
        };
      };

      fileSystems."/" = { device = "/dev/sda"; fsType = "ext4"; };

      swapDevices = [ { device = "/dev/sdb"; } ];
    };

    resources.ec2KeyPairs.keyPair = { inherit region; };
  }
