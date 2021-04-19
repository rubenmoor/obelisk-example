{ system ? builtins.currentSystem
, pkgs ? import <nixpkgs> {}
, obelisk ? import ./.obelisk/impl {
  reflex-platform-func = args@{ ... }: import ../reflex-platform (args // {
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

    # In order to use Let's Encrypt for HTTPS deployments you must accept
    # their terms of service at https://letsencrypt.org/repository/.
    # Uncomment and set this to `true` to indicate your acceptance:
    # terms.security.acme.acceptTerms = false;
  }
}:
with obelisk;
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
in
  project ./. ({ ... }: {
    android.applicationId = "systems.obsidian.obelisk.examples.minimal";
    android.displayName = "Obelisk Minimal Example";
    ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
    ios.bundleName = "Obelisk Minimal Example";
    overrides = self: super: {
      gerippe = pkgs.haskell.lib.dontHaddock (self.callCabal2nix "gerippe" ../gerippe { });
      hspec-snap = self.callCabal2nix "hspec-snap" (pkgs.fetchFromGitHub {
        owner = "dbp";
        repo = "hspec-snap";
        rev = "60216c2fe435f157de4e253df0d318ecc1dfdaab";
        sha256 = "08i8462pp76p1kkrpa1968ky7y9jbxz4073qmx9l8r83wqwwjxkk";
      }) {};
      persistent = self.callHackage "persistent" "2.9.2" {};
      reflex-dom = self.callCabal2nix "reflex-dom" (reflex-dom-framework + /reflex-dom) {};
      reflex-dom-core = pkgs.haskell.lib.dontCheck (
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
      servant-snap = self.callCabal2nix "servant-snap" (pkgs.fetchFromGitHub {
        owner = "haskell-servant";
        repo = "servant-snap";
        rev = "b54c5da86f2f2ed994e9dfbb0694c72301b5a220";
        sha256 = "0j0a3lznxnf8f98fibla7d0bksz3kk4z9q02afmls5f9yylpf2ad";
      }) {};
      password = self.callCabal2nix "password" (password-repo + /password) {};
      password-types = self.callCabal2nix "password-types"
        (password-repo + /password-types) {};
      base64 = self.callHackage "base64" "0.3.1.1" {};
      password-instances = self.callCabal2nix "password-instances"
        (password-repo + /password-instances) {};
      clay = self.callHackage "clay" "0.13.3" {};
    };
  })
