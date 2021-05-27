{ system ? builtins.currentSystem
, pkgs ? import <nixpkgs> {}
}:
let password-repo = pkgs.fetchFromGitHub {
      owner = "cdepillabout";
      repo = "password";
      rev = "6b243c282ca3d5fed50fbabe0f25bf152ad8f8cf";
      sha256 = "1qkhyd3x67dgag2xz32sd94n3zyhr1r2n0j8p2rlva74ar8qdgln";
    };
    reflex-dom-framework = pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-dom";
      rev = "6a7782a61e90e7369a8278441eb47f702bb7c63b";
      sha256 = "13y2h9cqhll55qgk7x33wnz88822irkdxych1c0fbw20jghhp96h";
    };
    reflex-platform-hls = pkgs.fetchFromGitHub {
      # branch: haskell-language-server
      owner = "ibizaman";
      repo = "reflex-platform";
      rev = "a9cd44d288395092fdaa76a6a7f146049cc21f15";
      sha256 = "1nfzls18cs0a1a0d3xiiidsl0n6clcf5z9i8b78yj1k8hdc6k1fh";
    };
    mkObeliskApp =
      { exe
      , routeHost
      , enableHttps
      , name ? "backend"
      , user ? name
      , group ? user
      , baseUrl ? "/"
      , internalPort ? 8000
      , backendArgs ? "--port=${toString internalPort}"
      , ...
      }: {...}: {
      services.nginx = {
        enable = true;
        virtualHosts."${routeHost}" = {
          enableACME = enableHttps;
          forceSSL = enableHttps;
          locations.${baseUrl} = {
            proxyPass = "http://127.0.0.1:" + toString internalPort;
            proxyWebsockets = true;
          };
        };
      };
      services.mysql = {
        enable = true;
        package = pkgs.mariadb;
        initialDatabases = [ { name = "podcast"; } ];
        ensureUsers = [ {
          name = user;
          ensurePermissions = { "podcast.*" = "ALL PRIVILEGES"; };
        } ];
        settings.mysqld = {
          character-set-server = "utf8mb4";
          collation-server = "utf8mb4_unicode_ci";
        };
      };
      systemd.services.${name} = {
        wantedBy = [ "multi-user.target" ];
        after = [ "mysql.service" ];
        restartIfChanged = true;
        path = [ pkgs.gnutar ];
        script = ''
          ln -sft . '${exe}'/*
          mkdir -p log
          exec ./backend ${backendArgs} </dev/null
        '';
        serviceConfig = {
          User = user;
          KillMode = "process";
          WorkingDirectory = "~";
          Restart = "always";
          RestartSec = 5;
        };
      };
      users = {
        users.${user} = {
          description = "${user} service";
          home = "/var/lib/${user}";
          createHome = true;
          isSystemUser = true;
          group = group;
        };
        groups.${group} = {};
      };
    };
    obelisk = (import ./.obelisk/impl {
      reflex-platform-func = args@{ ... }: (import reflex-platform-hls) (args // {
          inherit system;
          # activate haskell-language-server for reflex-platform with full
          # support for template-haskell
          hlsSupport = true;
      });
      inherit system;
      iosSdkVersion = "13.2";

      # You must accept the Android Software Development Kit License Agreement at
      # https://developer.android.com/studio/terms in order to build Android apps.
      # Uncomment and set this to `true` to indicate your acceptance:
      # config.android_sdk.accept_license = false;

      terms.security.acme.acceptTerms = true;
    }) // { inherit mkObeliskApp; };
in
  with obelisk;
  with pkgs.haskell.lib;
  project ./. ({ ... }: {
    android.applicationId = "systems.obsidian.obelisk.examples.minimal";
    android.displayName = "Obelisk Minimal Example";
    ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
    ios.bundleName = "Obelisk Minimal Example";
    staticFiles = import ./static.nix { inherit pkgs; };
    overrides = self: super: {
      gerippe = dontHaddock (self.callCabal2nix "gerippe" (pkgs.fetchFromGitHub {
        owner = "rubenmoor";
        repo = "gerippe";
        rev = "5dedac304b5378eea912f7ce1055793ee108d713";
        sha256 = "0jcd3bfm6kcy47iy0z1zbbl8asmy4kvbv1n01g52g550ksgssq5x";
      }) {});
      # hspec-snap = self.callCabal2nix "hspec-snap" (pkgs.fetchFromGitHub {
      #   owner = "dbp";
      #   repo = "hspec-snap";
      #   rev = "60216c2fe435f157de4e253df0d318ecc1dfdaab";
      #   sha256 = "08i8462pp76p1kkrpa1968ky7y9jbxz4073qmx9l8r83wqwwjxkk";
      # }) {};
      persistent = self.callHackage "persistent" "2.9.2" {};
      reflex-dom = self.callCabal2nix "reflex-dom" (reflex-dom-framework + /reflex-dom) {};
      reflex-dom-core = dontCheck (
        self.callCabal2nix "reflex-dom-core" (
          reflex-dom-framework + /reflex-dom-core
        ) {}
      );
      servant-reflex = self.callCabal2nix "servant-reflex" (pkgs.fetchFromGitHub {
        owner = "imalsogreg";
        repo = "servant-reflex";
        rev = "20e2621cc2eca5fe38f8a01c7a159b0b9be524ea";
        sha256 = "0aqyk04yg39xj40aj86hr6gwbzvj6i2fxi8zznmfl5fay8l96b4g";
      }) {};
      servant-snap = pkgs.haskell.lib.dontCheck (self.callCabal2nix "servant-snap" (pkgs.fetchFromGitHub {
        owner = "haskell-servant";
        repo = "servant-snap";
        rev = "b54c5da86f2f2ed994e9dfbb0694c72301b5a220";
        sha256 = "0j0a3lznxnf8f98fibla7d0bksz3kk4z9q02afmls5f9yylpf2ad";
      }) {});
      password = self.callCabal2nix "password" (password-repo + /password) {};
      password-types = self.callCabal2nix "password-types"
        (password-repo + /password-types) {};
      base64 = self.callHackage "base64" "0.3.1.1" {};
      password-instances = self.callCabal2nix "password-instances"
        (password-repo + /password-instances) {};
      clay = self.callHackage "clay" "0.13.3" {};
    };
  })
