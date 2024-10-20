{
  inputs = {
    lix-module = {
      url = "https://git.lix.systems/lix-project/nixos-module/archive/2.91.0.tar.gz";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-utils.url = "github:numtide/flake-utils";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ect = {
      url = "github:eviefp/ect";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixvim = {
      url = "github:nix-community/nixvim/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay/master";
      inputs.nixpkgs.follows = "nixpkgs";
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

    porc = {
      url = "github:soenkehahn/porc/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    gitu = {
      url = "github:altsem/gitu";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nixpkgs-stable.follows = "nixpkgs";
      };
    };

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };


    asus-wmi-screenpad = {
      url = "github:MatthewCash/asus-wmi-screenpad-module";
      inputs.nixpkgs.follows = "nixpkgs";
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

  };

  outputs =
    dotfiles:
    (dotfiles.flake-utils.lib.eachDefaultSystem
      (system:
      let
        pkgs = import dotfiles.nixpkgs {
          inherit system;
        };
        treefmt-config = {
          projectRootFile = "flake.nix";
          programs = {
            nixpkgs-fmt.enable = true;
          };
        };
        treefmt = (dotfiles.treefmt-nix.lib.evalModule pkgs treefmt-config).config.build;
      in
      {
        formatter = treefmt.wrapper;
        checks = {
          fmt = treefmt.check dotfiles.self;
        };
      })) //
    {
      nixosModules = import ./modules/nixos;
      homeManagerModules = import ./modules/home-manager;

      nixosConfigurations.thelxinoe = dotfiles.nixpkgs.lib.nixosSystem {
        # The host needs to pass 'dotfiles' to the home-manager module import,
        # which results in an infinite recursion error if this was replaced by
        # '_module.args'.
        # See https://nixos-and-flakes.thiscute.world/nixos-with-flakes/nixos-flake-and-module-system#pass-non-default-parameters-to-submodules
        specialArgs = { inherit dotfiles; };
        modules = [
          ./hosts/thelxinoe
        ];
      };

      nixosConfigurations."janus" = dotfiles.nixpkgs.lib.nixosSystem {
        specialArgs = { inherit dotfiles; };
        modules = [
          ./hosts/janus
        ];
      };

      nixosConfigurations."aiode" = dotfiles.nixpkgs.lib.nixosSystem {
        specialArgs = { inherit dotfiles; };
        modules = [
          ./hosts/aiode
        ];
      };

      nixosConfigurations."fractal" = dotfiles.nixpkgs.lib.nixosSystem {
        specialArgs = { inherit dotfiles; };
        modules = [
          ./hosts/fractal
        ];
      };

      nixosConfigurations."arche" = dotfiles.nixpkgs.lib.nixosSystem {
        specialArgs = { inherit dotfiles; };
        modules = [
          ./hosts/arche
        ];
      };

      nixOnDroidConfigurations.thanatos = dotfiles.nix-on-droid.lib.nixOnDroidConfiguration {
        modules = [
          ./hosts/thanatos
        ];
      };
    }
  ;
}
