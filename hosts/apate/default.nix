{ pkgs, dotfiles, theme, lib, ... }:
{
  imports = [
    dotfiles.home-manager.darwinModules.home-manager
    {
      home-manager = {
        backupFileExtension = "backup";
        useGlobalPkgs = true;
        useUserPackages = true;
        extraSpecialArgs = { inherit dotfiles theme; };
        users.evie = ./home.nix;
      };
    }
  ];

  config = {
    environment = {
      shells = [ pkgs.nushell ];
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
      overlays = [ (import dotfiles.emacs-overlay) dotfiles.nur.overlays.default ];
      hostPlatform = "x86_64-darwin";
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
