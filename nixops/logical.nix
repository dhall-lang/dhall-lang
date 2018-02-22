{ ipfs = { pkgs, ... }:

    let
      pin = { name, path }:
          { name = "pin-${name}";

            value = {
              path = [ pkgs.bash ];

              script = ''
                IPFS_PATH="/var/lib/ipfs/" ${pkgs.ipfs}/bin/ipfs add --quiet -w --recursive ${path}
              '';

              serviceConfig = {
                Type = "oneshot";
              };

              wantedBy = [ "multi-user.target" ];
            };
          };

      pinPrelude = { date }:
        let
          path = ./. + "/dhall/${date}.json";

          json = builtins.fromJSON (builtins.readFile path);

          src = pkgs.fetchgit { inherit (json) url rev sha256; };
        in
          pin { name = date; path = "${src}/Prelude"; };

      services = [
        (pin { name = "True"; path = builtins.toFile "True" "True\n"; })
        (pinPrelude { date = "2016-12-03"; })
        (pinPrelude { date = "2017-05-16"; })
        (pinPrelude { date = "2017-06-17"; })
        (pinPrelude { date = "2017-08-28"; })
      ];

    in
      { networking.firewall.allowedTCPPorts = [ 22 4001 ];

        services = {
          fail2ban.enable = true;

          ipfs = {
            enable = true;

            enableGC = true;
          };
        };

        systemd.services = builtins.listToAttrs services // {
          ipfs.environment.IPFS_LOW_MEM = "1";
        };
      };

  hydra = { pkgs, ... }: {
    environment = {
      etc =
        let
          toProject = suffix: {
            name = "hydra/dhall-${suffix}.json";

            value = { text = builtins.toJSON (import ./project.nix suffix); };
          };

          suffixes = [ "bash" "haskell" "json" "nix" "text" "to-cabal" ];

        in
          builtins.listToAttrs (builtins.map toProject suffixes) // {
            "hydra/jobsets.nix".text = builtins.readFile ./jobsets.nix;

            "hydra/machines".text = ''
              hydra-queue-runner@hydra x86_64-linux /etc/keys/hydra-queue-runner/hydra-queue-runner_rsa 1 1 local
            '';
          };

      systemPackages = [ pkgs.hydra ];
    };

    networking.firewall.allowedTCPPorts = [ 22 80 443 5000 ];

    nix = {
      autoOptimiseStore = true;

      gc.automatic = true;
    };

    nixpkgs.overlays =
      let
        secureHydra = packagesNew: packagesOld: {
          hydra = packagesOld.hydra.overrideAttrs (oldAttributes: {
              patches = (oldAttributes.patches or []) ++ [ ./hydra.patch ];
            }
          );
        };

        fixSimple = packagesNew: packagesOld: {
          certbot = packagesOld.certbot.overrideAttrs (oldAttributes: rec {
              version = "0.19.0";

              src = packagesNew.fetchFromGitHub {
                owner = "certbot";

                repo = "certbot";

                rev = "v${version}";

                sha256 = "14i3q59v7j0q2pa1dri420fhil4h0vgl4vb471hp81f4y14gq6h7";
              };
            }
          );

          simp_le = packagesOld.simp_le.overrideAttrs (oldAttributes: {
              version = "0.6.1";

              src = oldAttributes.src.override {
                sha256 = "0x4fky9jizs3xi55cdy217cvm3ikpghiabysan71b07ackkdfj6k";
              };
            }
          );
        };

      in
        [ secureHydra fixSimple ];

    services = {
      fail2ban.enable = true;

      hydra = {
        buildMachinesFiles = [ "/etc/hydra/machines" ];

        enable = true;

        extraConfig = ''
          <githubstatus>
            jobs = .*
            inputs = src
            authorization = dhall-lang
            context = hydra
          </githubstatus>
        '';

        hydraURL = "https://hydra.dhall-lang.org";

        listenHost = "127.0.0.1";

        logo = ../img/dhall-logo.png;

        notificationSender = "noreply@dhall-lang.org";
      };

      nginx = {
        enable = true;

        recommendedGzipSettings = true;

        recommendedOptimisation = true;

        recommendedProxySettings = true;

        recommendedTlsSettings = true;

        virtualHosts."dhall-lang.org" = {
          addSSL = true;

          default = true;

          enableACME = true;

          locations."/".extraConfig = ''
            rewrite ^.*$ https://github.com/dhall-lang/dhall-lang/blob/master/README.md redirect;
          '';
        };

        virtualHosts."cache.dhall-lang.org" = {
          addSSL = true;

          enableACME = true;

          locations."/".proxyPass = "http://127.0.0.1:5000";
        };

        virtualHosts."hydra.dhall-lang.org" = {
          addSSL = true;

          enableACME = true;

          locations."/".proxyPass = "http://127.0.0.1:3000";
        };

        virtualHosts."prelude.dhall-lang.org" = {
          locations."/".extraConfig = ''
            rewrite ^/(.*)$ https://ipfs.io/ipfs/QmQ8w5PLcsNz56dMvRtq54vbuPe9cNnCCUXAQp6xLc6Ccx/Prelude/$1 redirect;
          '';
        };
      };

      nix-serve = {
        bindAddress = "127.0.0.1";

        enable = true;

        secretKeyFile = "/etc/nix-serve/nix-serve.sec";
      };

      openssh.enable = true;
    };

    systemd.services = {
      generate-hydra-queue-runner-key-pair = {
        script =
          let
            keyDirectory = "/etc/keys/hydra-queue-runner";

            user = "hydra-queue-runner";

            group = "hydra";

            privateKey = "${keyDirectory}/${user}_rsa";

            publicKey = "${privateKey}.pub";

            authorizedKeysDirectory = "/etc/ssh/authorized_keys.d";

            authorizedKeysFile = "${authorizedKeysDirectory}/${user}";
          in
            ''
              if ! [ -e ${privateKey} ] || ! [ -e ${publicKey} ]; then
                mkdir -p ${keyDirectory}

                ${pkgs.openssh}/bin/ssh-keygen -t rsa -N "" -f ${privateKey} -C "${user}@hydra" >/dev/null

                chown -R ${user}:${group} ${keyDirectory}
              fi

              if ! [ -e ${authorizedKeysFile} ]; then
                mkdir -p "${authorizedKeysDirectory}"

                cp ${publicKey} ${authorizedKeysFile}
              fi
            '';

        serviceConfig.Type = "oneshot";

        wantedBy = [ "multi-user.target" ];
      };

      nix-serve-keys = {
        script =
          let
            keyDirectory = "/etc/nix-serve";

            privateKey = "${keyDirectory}/nix-serve.sec";

            publicKey = "${keyDirectory}/nix-serve.pub";

          in
            ''
              if [ ! -e ${keyDirectory} ]; then
                mkdir -p ${keyDirectory}
              fi

              if ! [ -e ${privateKey} ] || ! [ -e ${publicKey} ]; then
                ${pkgs.nix}/bin/nix-store --generate-binary-cache-key cache.dhall-lang.org ${privateKey} ${publicKey}
              fi

              chown -R nix-serve:hydra /etc/nix-serve
            '';

        serviceConfig.Type = "oneshot";

        wantedBy = [ "multi-user.target" ];
      };
    };
  };
}
