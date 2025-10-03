{
  pkgs,
  dotfiles,
  theme,
  lib,
  ...
}: let
  chromium-overlay = self: super: {
    chromium = super.stdenv.mkDerivation rec {
      version = "1368521";

      name = "Chromium-${version}";
      buildInputs = [super.unzip];
      sourceRoot = ".";
      phases = [
        "unpackPhase"
        "installPhase"
      ];
      installPhase = ''
        mkdir -p "$out/Applications"
        cp -r chrome-mac/Chromium.app "$out/Applications/Chromium.app"
      '';

      src = super.fetchurl {
        name = "Mac_Arm_${version}_chrome-mac.zip";
        url = "https://www.googleapis.com/download/storage/v1/b/chromium-browser-snapshots/o/Mac_Arm%2F${version}%2Fchrome-mac.zip?generation=1728953032008580&alt=media";
        sha256 = "86ed1b3b90886c3ba0666b8e330681f77fe5240aa49da1375059029e9b58f12b";
      };

      meta = with super.stdenv.lib; {
        description = "Chromium";
        homepage = "http://www.chromium.org";
        maintainers = with super.maintainers; [robertogoam];
        platforms = ["aarch64-darwin"];
      };
    };
  };
in {
  imports = [
    dotfiles.home-manager.darwinModules.home-manager
    {
      home-manager = {
        backupFileExtension = "backup";
        useGlobalPkgs = true;
        useUserPackages = true;
        extraSpecialArgs = {inherit dotfiles theme;};
        users.evie = ./home.nix;
      };
    }
  ];

  config = {
    environment = {
      shells = [pkgs.nushell];
    };

    networking = {
      computerName = "apate";
      hostName = "apate";
    };

    nix = {
      channel.enable = false;
      settings = {
        trusted-substituters = [
          "https://nix-community.cachix.org"
          "https://cache.lix.systems"
        ];
        trusted-public-keys = [
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          "cache.lix.systems:aBnZUw8zA7H35Cz2RyKFVs3H4PlGTLawyY5KRbvJR8o="
        ];
        extra-experimental-features = [
          "flakes"
          "nix-command"
        ];
        nix-path = lib.mkForce "nixpkgs=${dotfiles.nixpkgs}";
      };
    };

    nixpkgs = {
      overlays = [(import dotfiles.emacs-overlay) dotfiles.nur.overlays.default chromium-overlay];
      hostPlatform = "aarch64-darwin";
      config.allowUnfree = true;
    };

    security = {
      pam.services.sudo_local.touchIdAuth = true;
    };

    services = {
      openssh.enable = true;
    };

    users.users.evie = {
      home = "/Users/evie";
      isHidden = false;
      shell = pkgs.nushell;
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDP+cKsCIfQyFN06ZX1VhfsHvu7FT3F3cwY7fmnZ8kIjmhSz/uYlG4wIJikeZhqWrMsu4ozQaiWdl0zKsw3KpQyyJ8alhfxAtOeBkuifL61Tnq4QOQ2y8/8zziD5eVD2WY2Nw4n8+fT13pAK/117WWOpoWMC1CbRr0YZWoobkABhOIVKDMWj+pUgbmMTAbg1rCyRuQYrlo5dsdK/mWQJRnOFUK0u9mx/lu2Iz22pU1oBxwSJQ2FhyDX/DSKG732paSblC7o4GwiH5uIKmJPDUtT4LCZy9j5DpxD2q5HcQAgsA1xRTSxnAg+8gJXo1BSX7eepLwBtAJJq6IuRKxIRlvq4HA5WEfLvpddNzMOVDNWmVi1i+oO6xZNDbxOUat0G36MzAdhExsBmwiw5ga9soOMu/QqNz4vAQ3/Hnyh17AntrkjekyYoT1nXrGKS+f+sGjoFNDsWwdfDpiDn6B5nCrIb5b5eX3XmCKz1NVLspKyELVVCmFdJfEvxhnFOevNOW3kWSwtQW4VJshcIR8NydkEjWjcezWeIChym+bFBmgjDjYwOxFGyRHDooPwXhmeQDORA7mY/az6tQLSk0UTeoLmavcUI73Qv8aNtuXrAWTG4BJ2rf/+yvs3ygpckZGydq9L7xtFHqHGY3gOnGjcFAp8xnsSVJE5h32/PDxi6j+HBw== cardno:31_472_472"
      ];
    };

    system.stateVersion = 6;
  };
}
