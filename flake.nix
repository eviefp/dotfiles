{
  inputs = {
    # https://git.lix.systems/lix-project/lix/issues/943
    # lix-module = {
    #   url = "https://git.lix.systems/lix-project/nixos-module/archive/2.93.3-1.tar.gz";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };

    lix = {
      url = "https://git.lix.systems/lix-project/lix/archive/main.tar.gz";
      flake = false;
    };

    lix-module = {
      url = "https://git.lix.systems/lix-project/nixos-module/archive/main.tar.gz";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.lix.follows = "lix";
    };

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    nixpkgs2411.url = "github:NixOS/nixpkgs/nixos-24.11-small";

    nix-darwin = {
      url = "github:nix-darwin/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

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
      inputs.nixpkgs.follows = "nixpkgs";
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

    nix-minecraft = {
      url = "github:Infinidoge/nix-minecraft";
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
        pkgs-2411 = import dotfiles.nixpkgs2411 {
          inherit system;
        };
        treefmt-config = {
          projectRootFile = "flake.nix";
          programs = {
            nixpkgs-fmt.enable = true;
            cabal-fmt.enable = true;
            deadnix.enable = true;
            fourmolu = {
              enable = true;
              package = pkgs.haskell.packages.ghc9102.fourmolu;
            };
            hlint.enable = true;
            mdformat.enable = true;
            shellcheck.enable = true;
            jsonfmt.enable = true;
          };
        };
        treefmt = (dotfiles.treefmt-nix.lib.evalModule pkgs treefmt-config).config.build;
        haskellPackages = pkgs.haskell.packages.ghc9102.override (prevArgs: {
          overrides = pkgs.lib.composeExtensions (prevArgs.overrides or (_: _: { })) (final: _prev: {
            dotfiles-script = final.callCabal2nix "dotfiles-script" ./scripts/dotfiles-script { };
          });
        });
      in
      {
        formatter = treefmt.wrapper;

        checks = {
          fmt = treefmt.check dotfiles.self;
        };

        packages = import ./packages { inherit pkgs; inherit pkgs-2411; };

        devShells.default = haskellPackages.shellFor {
          packages = p: [ p.dotfiles-script ];
          buildInputs = [
            pkgs.haskell.compiler.ghc9102
            pkgs.haskell.packages.ghc9102.cabal-install
            pkgs.haskell.packages.ghc9102.haskell-language-server
            pkgs.zlib.dev
          ];
        };

      })) //
    rec {
      nixosModules = import ./modules/nixos;
      homeModules = import ./modules/home-manager;

      lib =
        let
          pkgs = import dotfiles.nixpkgs {
            system = "x86_64-linux";
          };
        in
        import ./lib { inherit pkgs; };

      nixosConfigurations.thelxinoe = dotfiles.nixpkgs.lib.nixosSystem {
        # The host needs to pass 'dotfiles' to the home-manager module import,
        # which results in an infinite recursion error if this was replaced by
        # '_module.args'.
        # See https://nixos-and-flakes.thiscute.world/nixos-with-flakes/nixos-flake-and-module-system#pass-non-default-parameters-to-submodules
        specialArgs = { inherit dotfiles; theme = lib.theme.default; };
        modules = [
          ./hosts/thelxinoe
        ];
      };

      nixosConfigurations = {
        "janus" = dotfiles.nixpkgs.lib.nixosSystem {
          specialArgs = { inherit dotfiles; theme = lib.theme.default; };
          modules = [
            ./hosts/janus
          ];
        };

        "aiode" = dotfiles.nixpkgs.lib.nixosSystem {
          specialArgs = { inherit dotfiles; theme = lib.theme.default; };
          modules = [
            ./hosts/aiode
          ];
        };

        "fractal" = dotfiles.nixpkgs.lib.nixosSystem {
          specialArgs = { inherit dotfiles; theme = lib.theme.default; };
          modules = [
            ./hosts/fractal
          ];
        };

        "arche" = dotfiles.nixpkgs.lib.nixosSystem {
          specialArgs = { inherit dotfiles; theme = lib.theme.default; };
          modules = [
            ./hosts/arche
          ];
        };

        "jellyfin" = dotfiles.nixpkgs.lib.nixosSystem {
          specialArgs = { inherit dotfiles; theme = lib.theme.default; };
          modules = [
            ./hosts/jellyfin
          ];
        };
      };

      darwinConfigurations."apate" = dotfiles.nix-darwin.lib.darwinSystem {
        specialArgs = { inherit dotfiles; theme = lib.theme.default; };
        modules = [
          ./hosts/apate
        ];
      };

    }

  ;
}
