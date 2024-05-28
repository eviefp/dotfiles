{
  inputs = {
    lix = {
      url = "git+https://git@git.lix.systems/lix-project/lix?ref=refs/tags/2.90-beta.1";
      flake = false;
    };

    lix-module = {
      url = "git+https://git.lix.systems/lix-project/nixos-module";
      inputs.lix.follows = "lix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ect = {
      url = "github:eviefp/ect";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    lean4-mode = {
      url = "github:leanprover/lean4-mode";
      flake = false;
    };

    nil = {
      url = "github:oxalica/nil/main";
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

    # Hyprland stuff
    hyprland = {
      url = "git+https://github.com/hyprwm/Hyprland?submodules=1";
    };

    hyprpaper = {
      url = "github:hyprwm/hyprpaper";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprpicker = {
      url = "github:hyprwm/hyprpicker";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hypridle = {
      url = "github:hyprwm/hypridle";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprlock = {
      url = "github:hyprwm/hyprlock";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprcursor = {
      url = "github:hyprwm/hyprcursor";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nixpkgs-stable.follows = "nixpkgs";
      };
    };

  };

  outputs =
    { lix-module, nixpkgs, home-manager, nix-on-droid, nix-neovim, emacs-overlay, nil, lean4-mode, hyprland, hyprpaper, hyprpicker, hypridle, hyprlock, hyprcursor, ect, sops-nix, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [ (import emacs-overlay) ];
      };
      aarch-pkgs = import nixpkgs {
        system = "aarch64-linux";

        overlays = [
          nix-on-droid.overlays.default
          (import emacs-overlay)
        ];
      };
      nix-path = "nixpkgs=${nixpkgs}";
      home-manager-special-args = {
        inherit pkgs nix-path nix-neovim nil lean4-mode hyprland hyprpaper hyprpicker hypridle hyprlock hyprcursor ect sops-nix;
      };
    in
    {
      nixosConfigurations."thelxinoe" = nixpkgs.lib.nixosSystem {
        system = system;
        specialArgs = { inherit hyprland; };
        modules = [
          lix-module.nixosModules.default
          ./system/thelxinoe/configuration.nix
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              extraSpecialArgs = home-manager-special-args;
              users.evie = import ./home-manager/thelxinoe/home.nix;
            };
          }
        ];
      };

      nixosConfigurations."janus" = nixpkgs.lib.nixosSystem {
        system = system;
        specialArgs = { inherit hyprland; };
        modules = [
          lix-module.nixosModules.default
          ./system/janus/configuration.nix
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              extraSpecialArgs = home-manager-special-args;
              users.evie = import ./home-manager/janus/home.nix;
            };
          }
        ];
      };

      nixosConfigurations."aiode" = nixpkgs.lib.nixosSystem {
        system = system;
        specialArgs = { inherit hyprland; };
        modules = [
          lix-module.nixosModules.default
          ./system/aiode/configuration.nix
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              extraSpecialArgs = home-manager-special-args;
              users.evie = import ./home-manager/aiode/home.nix;
            };
          }
        ];
      };

      nixosConfigurations."fractal" = nixpkgs.lib.nixosSystem {
        system = system;
        modules = [
          lix-module.nixosModules.default
          ./system/fractal/configuration.nix
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              extraSpecialArgs = home-manager-special-args;
              users.evie = import ./home-manager/fractal/home.nix;
            };
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
              extraSpecialArgs = {
                inherit nix-path nix-neovim;
                pkgs = aarch-pkgs;
              };
            };
          }
        ];

        ## Different pkgs, need to use the nix-on-droid overlay
        pkgs = aarch-pkgs;

      };
    };
}
