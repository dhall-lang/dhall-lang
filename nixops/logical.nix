{ config, pkgs, ... }: {
  imports =
    let
      nixos-mailserver =
        builtins.fetchTarball {
          url = "https://gitlab.com/simple-nixos-mailserver/nixos-mailserver/-/archive/v2.2.1/nixos-mailserver-v2.2.0.tar.gz";

          sha256 = "03d49v8qnid9g9rha0wg2z6vic06mhp0b049s3whccn1axvs2zzx";
        };

    in
      [ nixos-mailserver ];

  environment = {
    etc =
      let
        toProject = suffix: {
          name = "hydra/dhall-${suffix}.json";

          value = { text = builtins.toJSON (import ./project.nix suffix); };
        };

        suffixes = [
          "bash"
          "haskell"
          "json"
          "kubernetes"
          "lang"
          "nix"
          "text"
          "to-cabal"
        ];

      in
        builtins.listToAttrs (builtins.map toProject suffixes) // {
          "hydra/jobsets.nix".text = builtins.readFile ./jobsets.nix;

          "hydra/machines".text = ''
            hydra-queue-runner@dhall-lang.org x86_64-linux,builtin /etc/keys/hydra-queue-runner/hydra-queue-runner_rsa 4 1 local,big-parallel
          '';
        };

    systemPackages = [ pkgs.hydra ];
  };

  mailserver = {
    enable = true;

    certificateScheme = 3;

    domains = [ "discourse.dhall-lang.org" "dhall-lang.org" ];

    enableImap = true;
    enableImapSsl = true;
    enablePop3 = true;
    enablePop3Ssl = true;

    fqdn = "mail.dhall-lang.org";

    loginAccounts = {
      "discourse@dhall-lang.org" = {
        hashedPassword = "$6$.vCIyiyMp4/HzO$FyW1OMFpL7tmPQvfDs2wDfhn5qTZW4NePBfDsrFQxPepIISrFYyjISeZS2kAHO6F/AolD7sus2mJUXsQUgCfv0";

        aliases = [ "postmaster@dhall-lang.org" ];
      };
    };

    policydSPFExtraConfig = ''
      skip_addresses = 172.17.0.2/32
    '';
  };

  networking.firewall.allowedTCPPorts = [ 22 80 443 ];

  nix = {
    autoOptimiseStore = true;

    gc.automatic = true;

    useSandbox = false;

    trustedUsers = [ "gabriel" ];
  };

  nixpkgs.overlays =
    let
      modifyHydra = packagesNew: packagesOld: {
        hydra = packagesOld.hydra.overrideAttrs (old: {
            patches = (old.patches or []) ++ [
              ./hydra.patch
              ./no-restrict-eval.patch
            ];
          }
        );
      };

    in
      [ modifyHydra ];

  programs.ssh.knownHosts = [
    { hostNames = [ "github.com" ];

      publicKey = "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAq2A7hRGmdnm9tUDbO9IDSwBK6TbQa+PXYPCPy6rbTrTtw7PHkccKrpp0yVhp5HdEIcKr6pLlVDBfOLX9QUsyCOV0wzfjIJNlGEYsdlLJizHhbn2mUjvSAHQqZETYP81eFzLQNnPHt4EVVUh7VfDESU84KezmD5QlWpXLmvU31/yMf+Se8xhHTvKSCZIFImWwoG6mbUoWf9nzpIoaSjB+weqqUUmpaaasXVal72J+UX2B+2RPW3RcT0eOzQgqlJL3RKrTJvdsjE3JEAvGq3lGHSZXy28G3skua2SmVi/w4yCE6gbODqnTWlg7+wC604ydGXA8VJiS5ap43JXiUFFAaQ==";
    }

    { hostNames = [ "dhall-lang.org" ];

      publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBp/WR0q2LUjpzHDwm03CijnpUyvHS9CDnJYvR0YNBpT";
    }
  ];

  security.sudo.wheelNeedsPassword = false;

  services = {
    fail2ban.enable = true;

    hydra = {
      buildMachinesFiles = [ "/etc/hydra/machines" ];

      enable = true;

      extraConfig = ''
        <githubstatus>
          jobs = .*:.*:dhall.*
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

    journald.extraConfig = ''
      SystemMaxUse=100M
    '';

    logrotate = {
      enable = true;

      config = ''
        /var/spool/nginx/logs/*.log {
          create 0644 nginx nginx
          daily
          rotate 7
          missingok
          notifempty
          compress
          sharedscripts
          postrotate
            kill -USR1 "$(${pkgs.coreutils}/bin/cat /run/nginx.pid 2>/dev/null)" > /dev/null || true
          endscript
        }
      '';
    };

    nginx = {
      enable = true;

      appendConfig = ''
        pid /run/nginx.pid;
      '';

      recommendedGzipSettings = true;

      recommendedOptimisation = true;

      recommendedTlsSettings = true;

      virtualHosts =
        let
          latestRelease = "v15.0.0";

          prelude = {
            forceSSL = true;

            enableACME = true;

            locations."/" = {
              extraConfig = ''
                if ($request_method = 'OPTIONS') {
                  add_header 'Access-Control-Allow-Origin' "*";
                  add_header 'Access-Control-Allow-Methods' 'GET, OPTIONS';
                  add_header 'Access-Control-Allow-Headers' 'DNT,User-Agent,X-Requested-With,If-Modified-Since,Cache-Control,Content-Type,Range';
                  add_header 'Access-Control-Max-Age' 600;
                  add_header 'Content-Type' 'text/plain; charset=utf-8';
                  add_header 'Content-Length' 0;
                  return 204;
                }
                if ($request_method = 'GET') {
                  add_header 'Access-Control-Allow-Methods' 'GET, OPTIONS';
                  add_header 'Access-Control-Allow-Headers' 'DNT,User-Agent,X-Requested-With,If-Modified-Since,Cache-Control,Content-Type,Range';
                  add_header 'Access-Control-Expose-Headers' 'Content-Length,Content-Range';
                }

                rewrite ^/?$ https://github.com/dhall-lang/dhall-lang/tree/${latestRelease}/Prelude redirect;
                rewrite ^/(v[^/]+)$ https://github.com/dhall-lang/dhall-lang/tree/$1/Prelude redirect;
                rewrite ^/(v[^/]+)/(.*)$ /dhall-lang/dhall-lang/$1/Prelude/$2 break;
                rewrite ^/(.*)$ /dhall-lang/dhall-lang/${latestRelease}/Prelude/$1 break;
              '';
              proxyPass = "https://raw.githubusercontent.com";
            };
          };

        in
          { "dhall-lang.org" =
              let
                inherit (pkgs) website;

              in
              { forceSSL = true;

                default = true;

                enableACME = true;

                locations."/" = {
                  index = "index.html";

                  root = "${website}";
                };
              };

          "cache.dhall-lang.org" = {
            forceSSL = true;

            enableACME = true;

            locations."/".proxyPass = "http://127.0.0.1:5000";
          };

          "discourse.dhall-lang.org" = {
            forceSSL = true;

            extraConfig = ''
              proxy_set_header Host $host;
              proxy_set_header X-Real-IP $remote_addr;
              proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
              proxy_set_header X-Forwarded-Proto $scheme;
              proxy_set_header X-Forwarded-Host $host;
              proxy_set_header X-Forwarded-Server $host;
              proxy_set_header Accept-Encoding "";
            '';

            enableACME = true;

            locations."/".proxyPass = "http://unix:/var/discourse/shared/standalone/nginx.http.sock";
          };

          "docs.dhall-lang.org" =
            let
              dhall-lang-derivations = import ../release.nix { };

              inherit (dhall-lang-derivations) docs;

            in
              { forceSSL = true;

                enableACME = true;

                locations."/" = {
                  index = "index.html";

                  root = "${docs}";
                };
              };

          "hydra.dhall-lang.org" = {
            forceSSL = true;

            extraConfig = ''
              proxy_set_header Host $host;
              proxy_set_header X-Real-IP $remote_addr;
              proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
              proxy_set_header X-Forwarded-Proto $scheme;
              proxy_set_header X-Forwarded-Host $host;
              proxy_set_header X-Forwarded-Server $host;
              proxy_set_header Accept-Encoding "";
            '';

            enableACME = true;

            locations."/".proxyPass = "http://127.0.0.1:3000";
          };

          "prelude.dhall-lang.org" = prelude;

          # Same as `prelude.dhall-lang.org` except requires a header named
          # `Test` to be present to authorize requests.  This is used to test
          # support for header forwarding for the test suite.
          "test.dhall-lang.org" =
            pkgs.lib.mkMerge
              [ prelude
                { locations."/".extraConfig = ''
                    if ($http_test = "") {
                      return 403;
                    }
                  '';
                }
              ];
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
    discourse =
      let
        discourseDirectory = "/var/discourse_docker";

        varDirectory = dirOf discourseDirectory;

        discourseSmtpPassword =
          let
            path = ./discourseSmtpPassword;

          in
            if builtins.pathExists path then builtins.readFile path else "";

        discourseConfiguration =
          pkgs.writeText "discourse-app.yaml" ''
            templates:
              - "templates/postgres.template.yml"
              - "templates/redis.template.yml"
              - "templates/web.template.yml"
              - "templates/web.ratelimited.template.yml"
              - "templates/web.socketed.template.yml"

            expose: []

            params:
              db_default_text_search_config: "pg_catalog.english"

              db_shared_buffers: "256MB"

              db_work_mem: "40MB"

              version: 41f09ee29c44424901054a7dbe0bf8c0a9b86742

            env:
              LANG: en_US.UTF-8

              UNICORN_WORKERS: 1

              DISCOURSE_HOSTNAME: 'discourse.dhall-lang.org'

              DISCOURSE_DEVELOPER_EMAILS: 'Gabriel439@gmail.com'

              DISCOURSE_SMTP_ADDRESS: mail.dhall-lang.org

              DISCOURSE_SMTP_USER_NAME: discourse@dhall-lang.org

              DISCOURSE_SMTP_PASSWORD: '${discourseSmtpPassword}'

              # TODO: Use CDN?
              #
              #DISCOURSE_CDN_URL: https://discourse-cdn.example.com

            volumes:
              - volume:
                  host: /var/discourse/shared/standalone
                  guest: /shared
              - volume:
                  host: /var/discourse/shared/standalone/log/var-log
                  guest: /var/log

            hooks:
              after_code:
                - exec:
                    cd: $home/plugins
                    cmd:
                      - git clone https://github.com/discourse/docker_manager.git

            run: []
          '';

      in
        { after = [ "network.target" "docker.service" ];

          path = [ pkgs.git pkgs.nettools pkgs.which pkgs.gawk pkgs.docker ];

          script = ''
            if [[ ! -e ${discourseDirectory} ]]; then
              ${pkgs.coreutils}/bin/mkdir --parents ${varDirectory}

              cd ${varDirectory}

              ${pkgs.git}/bin/git clone https://github.com/discourse/discourse_docker.git
            fi

            cd ${discourseDirectory}

            ${pkgs.git}/bin/git fetch https://github.com/discourse/discourse_docker.git 77edaf675a47729bb693d09b94713a2a98b5d686

            ${pkgs.git}/bin/git checkout FETCH_HEAD

            ${pkgs.coreutils}/bin/cp ${discourseConfiguration} ${discourseDirectory}/containers/app.yml

            ${pkgs.bash}/bin/bash ${discourseDirectory}/launcher rebuild app
          '';

          serviceConfig.RemainAfterExit = true;

          wantedBy = [ "multi-user.target" ];

          wants = [ "docker.service" ];
        };

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

    kick-hydra-evaluator = {
      script = ''
        ${pkgs.systemd}/bin/systemctl restart hydra-evaluator
      '';

      startAt = "*:0/5";

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

    self-deploy =
      let
        workingDirectory = "/var/lib/self-deploy";

        owner = "dhall-lang";

        repository = "dhall-lang";

        repositoryDirectory = "${workingDirectory}/${repository}";

        build = "${repositoryDirectory}/result";

      in
        { wantedBy = [ "multi-user.target" ];

          after = [ "network-online.target" ];

          path = [ pkgs.gnutar pkgs.gzip ];

          serviceConfig.X-RestartIfChanged = false;

          script = ''
            if [ ! -e ${workingDirectory} ]; then
              ${pkgs.coreutils}/bin/mkdir --parents ${workingDirectory}
            fi

            if [ ! -e ${repositoryDirectory} ]; then
              cd ${workingDirectory}
              ${pkgs.git}/bin/git clone git@github.com:${owner}/${repository}.git
            fi

            cd ${repositoryDirectory}

            ${pkgs.git}/bin/git fetch https://github.com/dhall-lang/dhall-lang.git master

            ${pkgs.git}/bin/git checkout FETCH_HEAD

            ${pkgs.nix}/bin/nix-build --attr machine ${repositoryDirectory}/release.nix

            ${pkgs.nix}/bin/nix-env --profile /nix/var/nix/profiles/system --set ${build}

            ${pkgs.git}/bin/git gc --prune=all

            ${build}/bin/switch-to-configuration switch
          '';

          startAt = "hourly";
        };
  };

  users.users.gabriel = {
    isNormalUser = true;

    extraGroups = [ "wheel" ];

    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGquu14+nGeuczn/u9wr2TD8L123DMOGLutPpXMDgMz5 gabriel@chickle"
    ];
  };

  virtualisation.docker = {
    enable = true;

    storageDriver = "overlay2";
  };
}
