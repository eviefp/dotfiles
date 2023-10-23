{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-on-droid = {
      url = "github:nix-community/nix-on-droid/master";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };

    nix-neovim = {
      url = "github:eviefp/nix-neovim/main";
    };

  };

  outputs =
    inputs@{ self, nixpkgs, home-manager, nix-on-droid, nix-neovim, emacs-overlay }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [ (import emacs-overlay) ];
      };
      nix-path = "nixpkgs=${nixpkgs}";
    in
    {
      nixosConfigurations."thelxinoe" = nixpkgs.lib.nixosSystem {
        system = system;
        modules =
          [
            ./system/thelxinoe/configuration.nix
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.extraSpecialArgs = { inherit pkgs nix-path nix-neovim; };
              home-manager.users.evie = import ./home-manager/thelxinoe/home.nix;
            }
          ];
      };

      nixosConfigurations."janus" = nixpkgs.lib.nixosSystem {
        system = system;
        modules =
          [
            ./system/janus/configuration.nix
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.extraSpecialArgs = { inherit pkgs nix-path nix-neovim; };
              home-manager.users.evie = import ./home-manager/janus/home.nix;
            }
          ];
      };

      nixOnDroidConfigurations.thanatos = nix-on-droid.lib.nixOnDroidConfiguration {
        modules = [
          ./system/thanatos/configuration.nix
          {
            home-manager = {
              config = ./home-manager/thanatos/home.nix;
              useGlobalPkgs = true;
              extraSpecialArgs = { inherit pkgs nix-path nix-neovim; };
            };
          }

        ];

        ## Different pkgs, need to use the nix-on-droid overlay
        pkgs = import nixpkgs {
          system = "aarch64-linux";

          overlays = [
            nix-on-droid.overlays.default
            (import emacs-overlay)
          ];
        };

      };
    };
}
