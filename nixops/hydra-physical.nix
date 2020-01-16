let
  hydraNixpkgs =
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/2437bb394392322d28d4244a63ab9b7ae8cc18dd.tar.gz";

      sha256 = "1c26n0z2zsapc1hxww1rpj056f8ah76f11h4f6wqjzjlj40i8jrq";
    };

in
  { ... }: {
    imports = [ "${hydraNixpkgs}/nixos/modules/profiles/qemu-guest.nix" ];

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
  }
