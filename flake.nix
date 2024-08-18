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
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nixpkgs-stable.follows = "nixpkgs";
      };
    };

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
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
      nixosModules = {
        common = ./modules/nixos/common.nix;
        boot = ./modules/nixos/boot.nix;
        hardware = ./modules/nixos/hardware.nix;
        locale = ./modules/nixos/locale.nix;
        network = ./modules/nixos/network.nix;
        nix-settings = ./modules/nixos/nix-settings.nix;
        peroxide = ./modules/nixos/peroxide.nix;
        packages = ./modules/nixos/packages.nix;
        services = ./modules/nixos/services.nix;
        users = ./modules/nixos/users.nix;
        wayland = ./modules/nixos/wayland.nix;
      };

      homeManagerModules = {
        common = ./modules/home-manager/common.nix;
        system = ./modules/home-manager/system.nix;
        fonts = ./modules/home-manager/fonts.nix;
        sops = ./modules/home-manager/sops.nix;

        email = ./modules/home-manager/email.nix;
        bower = ./modules/home-manager/programs/bower.nix;
        ect = ./modules/home-manager/programs/ect.nix;

        programs = {
          term = ./modules/home-manager/programs/term.nix;
          kitty = ./modules/home-manager/programs/kitty.nix;
          ranger = ./modules/home-manager/programs/shell/ranger.nix;

          browsers = ./modules/home-manager/programs/browsers.nix;
          chat = ./modules/home-manager/programs/chat.nix;
          streaming = ./modules/home-manager/programs/streaming.nix;

          dev = {
            default = ./modules/home-manager/programs/dev.nix;
            haskell = ./modules/home-manager/programs/dev/haskell.nix;
            lua = ./modules/home-manager/programs/dev/lua.nix;
            nix = ./modules/home-manager/programs/dev/nix.nix;
            provers = ./modules/home-manager/programs/dev/provers.nix;
            tools = ./modules/home-manager/programs/dev/tools.nix;
          };

          text = ./modules/home-manager/programs/text.nix;
        };

        editors = {
          emacs = ./modules/home-manager/programs/editors/emacs.nix;
          neovim = ./modules/home-manager/programs/editors/neovim.nix;
          helix = ./modules/home-manager/programs/editors/helix.nix;
        };

        gui = ./modules/home-manager/gui.nix;

        wayland = {
          default = ./modules/home-manager/wayland.nix;
          hyprland = {
            hyprland = ./modules/home-manager/wayland/hyprland/hyprland.nix;
            hyprpaper = ./modules/home-manager/wayland/hyprland/hyprpaper.nix;
            hypridle = ./modules/home-manager/wayland/hyprland/hypridle.nix;
            hyprlock = ./modules/home-manager/wayland/hyprland/hyprlock.nix;
          };
          river = ./modules/home-manager/wayland/river.nix;
          swaync = ./modules/home-manager/wayland/swaync.nix;
          eww = ./modules/home-manager/wayland/eww.nix;
          screenshot = ./modules/home-manager/wayland/screenshot.nix;
          rofi = ./modules/home-manager/wayland/rofi.nix;
        };
      };

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
