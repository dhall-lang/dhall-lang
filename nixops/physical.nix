let
  dhallLangNixpkgs = import ./dhallLangNixpkgs.nix;

in
  { ... }: {
    imports = [ "${dhallLangNixpkgs}/nixos/modules/profiles/qemu-guest.nix" ];

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

          version = 2;
        };

        timeout = 10;
      };
    };

    fileSystems."/" = { device = "/dev/sda"; fsType = "ext4"; };

    swapDevices = [ { device = "/dev/sdb"; } ];
  }
