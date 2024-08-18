{
  inputs = {
    lix-module = {
      url = "https://git.lix.systems/lix-project/nixos-module/archive/2.91.0.tar.gz";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

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
    dotfiles@{ lix-module, nixpkgs, home-manager, nix-on-droid, nix-neovim, emacs-overlay, nil, porc, gitu, lean4-mode, hyprland, hyprpaper, hyprpicker, hypridle, hyprlock, hyprcursor, ect, sops-nix, ... }:
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
        inherit pkgs nix-path nix-neovim nil porc gitu lean4-mode hyprland hyprpaper hyprpicker hypridle hyprlock hyprcursor ect sops-nix;
      };
    in
    {
      nixosModules = {
        common = ./system/modules/common.nix;
        boot = ./system/modules/boot.nix;
        hardware = ./system/modules/hardware.nix;
        locale = ./system/modules/locale.nix;
        network = ./system/modules/network.nix;
        nix-settings = ./system/modules/nix-settings.nix;
        peroxide = ./system/modules/peroxide.nix;
        packages = ./system/modules/packages.nix;
        services = ./system/modules/services.nix;
        users = ./system/modules/users.nix;
        wayland = ./system/modules/wayland.nix;
      };

      homeManagerModules = {
        common = ./home-manager/modules/common.nix;
        system = ./home-manager/modules/system.nix;
        fonts = ./home-manager/modules/fonts.nix;
        sops = ./home-manager/modules/sops.nix;

        email = ./home-manager/modules/email.nix;
        bower = home-manager/modules/programs/bower.nix;
        ect = home-manager/modules/programs/ect.nix;

        programs = {
          term = ./home-manager/modules/programs/term.nix;
          kitty = ./home-manager/modules/programs/kitty.nix;
          ranger = ./home-manager/modules/programs/shell/ranger.nix;

          browsers = ./home-manager/modules/programs/browsers.nix;
          chat = ./home-manager/modules/programs/chat.nix;
          streaming = ./home-manager/modules/programs/streaming.nix;

          dev = ./home-manager/modules/programs/dev.nix;
          devModules = {
            haskell = ./home-manager/modules/programs/dev/haskell.nix;
            lua = ./home-manager/modules/programs/dev/lua.nix;
            nix = ./home-manager/modules/programs/dev/nix.nix;
            provers = ./home-manager/modules/programs/dev/provers.nix;
            tools = ./home-manager/modules/programs/dev/tools.nix;
          };

          text = ./home-manager/modules/programs/text.nix;
        };

        editors = {
          emacs = ./home-manager/modules/programs/editors/emacs.nix;
          neovim = ./home-manager/modules/programs/editors/neovim.nix;
          helix = ./home-manager/modules/programs/editors/helix.nix;
        };

        gui = ./home-manager/modules/gui.nix;

        wayland = ./home-manager/modules/wayland.nix;
        waylandModules = {
          hyprland = {
            hyprland = import ./home-manager/modules/wayland/hyprland/default.nix;
            hyprpaper = ./home-manager/modules/wayland/hyprland/hyprpaper.nix;
            hypridle = ./home-manager/modules/wayland/hyprland/hypridle.nix;
            hyprlock = ./home-manager/modules/wayland/hyprland/hyprlock.nix;
          };
          swaync = home-manager/modules/wayland/swaync.nix;
          eww = home-manager/modules/wayland/eww.nix;
          screenshot = home-manager/modules/wayland/screenshot.nix;
          rofi = home-manager/modules/wayland/rofi.nix;
        };
      };

      nixosConfigurations.thelxinoe = nixpkgs.lib.nixosSystem {
        # The host needs to pass 'dotfiles' to the home-manager module import,
        # which results in an infinite recursion error if this was replaced by
        # '_module.args'.
        # See https://nixos-and-flakes.thiscute.world/nixos-with-flakes/nixos-flake-and-module-system#pass-non-default-parameters-to-submodules
        specialArgs = { inherit dotfiles; };
        modules = [
          ./hosts/thelxinoe
        ];
      };

      nixosConfigurations."arche" = nixpkgs.lib.nixosSystem {
        system = system;
        specialArgs = { inherit hyprland; };
        modules = [
          lix-module.nixosModules.default
          ./system/modules/common.nix
          ./system/modules/xserver.nix
          ./system/hardware/arche.nix
          {
            users.users.every = {
              isNormalUser = true;
              extraGroups = [ "wheel" "networkmanager" "video" "docker" "plugdev" "vboxusers" "input" "uinput" "lp" ];
              shell = pkgs.fish;
              hashedPassword =
                "$6$2bJFtErxPXqeCEJO$w4K0Fm1WmRL3tpUUJxkesiBFsM03Q2/IrtX9QvJjIBH3bxlOr1VtMIgWhCtIR1B./3QtmBCKo4H8ajTk51JW2/";
            };

            hardware = {
              xone.enable = false;
              xpadneo.enable = true;
              steam-hardware.enable = true;
            };

            services.desktopManager.plasma6.enable = true;

            evie.network = {
              hostName = "arche";
              interface = "enp0s31f6";
              extraPorts = [ 31234 ];
            };
            evie.xserver.useNVidia = true;
          }
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              extraSpecialArgs = home-manager-special-args;

              users.every = {
                imports = [
                  ./home-manager/modules/common.nix
                  ./home-manager/modules/gui.nix
                  ./home-manager/modules/programs/streaming.nix
                  ./home-manager/modules/programs/dev.nix
                ];

                evie = {
                  system.host = "arche";
                  system.user = "every";

                  programs.editors.emacs.locals = {
                    enable = true;
                    file = ./home-manager/locals/thelxinoe.el;
                  };
                };
              };
            };
          }
        ];
      };

      nixosConfigurations."janus" = nixpkgs.lib.nixosSystem {
        system = system;
        specialArgs = { inherit hyprland; };
        modules = [
          lix-module.nixosModules.default
          ./system/modules/common.nix
          ./system/modules/xserver.nix
          ./system/modules/logind.nix
          ./system/hardware/janus.nix
          {
            nixpkgs.config.packageOverrides = pkgs: {
              vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
            };
            evie.network = {
              hostName = "janus";
              wifi.enable = true;
            };
            evie.packages.extra = [ pkgs.libva pkgs.libva-utils ];
          }
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              extraSpecialArgs = home-manager-special-args;
              users.evie = {
                imports = [
                  ./home-manager/modules/common.nix
                  ./home-manager/modules/gui.nix
                  ./home-manager/modules/wayland.nix
                  ./home-manager/modules/programs/streaming.nix
                  # ./home-manager/modules/email.nix
                  ./home-manager/modules/sops.nix
                  ./home-manager/modules/programs/dev.nix
                ];

                evie = {
                  system.host = "janus";

                  programs.editors.emacs.locals = {
                    enable = true;
                    file = ./home-manager/locals/janus.el;
                  };

                  wayland = {
                    eww-monitor = "0";
                    showBattery = true;
                    useSshMailCalendar = true;
                    showMail = true;
                    showCalendar = true;
                    monitors = [
                      {
                        name = "DP-1";
                        resolution = "1920x515@60.075001";
                        position = "0x1080";
                        keybind = "E";
                      }
                      {
                        name = "eDP-1";
                        resolution = "1920x1080@60.05";
                        position = "0x0";
                        keybind = "W";
                      }
                    ];
                  };
                };

              };
            };
          }
        ];
      };

      nixosConfigurations."aiode" = nixpkgs.lib.nixosSystem {
        system = system;
        specialArgs = { inherit hyprland; };
        modules = [
          lix-module.nixosModules.default
          ./system/modules/common.nix
          ./system/modules/xserver.nix
          ./system/modules/logind.nix
          ./system/hardware/aiode.nix
          {
            evie.network = {
              hostName = "aiode";
              interface = "enp0s31f6";
              wifi = {
                enable = true;
                interface = "wlp2s0";
              };
            };
          }
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              extraSpecialArgs = home-manager-special-args;
              users.evie = {
                imports = [
                  ./home-manager/modules/common.nix
                  ./home-manager/modules/gui.nix
                  ./home-manager/modules/wayland.nix
                  ./home-manager/modules/programs/streaming.nix
                  # ./home-manager/modules/email.nix
                  ./home-manager/modules/sops.nix
                  ./home-manager/modules/programs/dev.nix
                ];

                evie = {
                  system.host = "aiode";

                  programs.editors.emacs.locals = {
                    enable = true;
                    file = ./home-manager/locals/aiode.el;
                  };

                  wayland = {
                    eww-monitor = "0";
                    showBattery = true;
                    useSshMailCalendar = true;
                    showMail = true;
                    showCalendar = true;
                    monitors = [
                      {
                        name = "eDP-1";
                        resolution = "1920x1080";
                        position = "0x0";
                        keybind = "W";
                      }
                    ];
                  };
                };

              };
            };
          }
        ];
      };

      nixosConfigurations."fractal" = nixpkgs.lib.nixosSystem {
        system = system;
        modules = [
          lix-module.nixosModules.default
          ./system/modules/common.nix
          ./system/hardware/fractal.nix
          {
            evie.boot.enableHeadless = true;
            evie.network = {
              hostName = "fractal";
              interface = "eno1";
              extraPorts = [ 1025 1143 ];
            };
            networking.firewall.allowedTCPPorts = [ 2049 ];

            services.nfs = {
              server = {
                enable = true;
                hostName = "fractal";
                exports = ''
                  /mnt/raid1 192.168.10.0/24(rw,async)
                '';
              };
            };

            evie.packages = { extra = [ pkgs.git pkgs.wget ]; };
          }
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              extraSpecialArgs = home-manager-special-args;
              users.evie = {
                imports = [
                  ./home-manager/modules/common.nix
                  ./home-manager/modules/programs/dev.nix
                ];

                evie = {
                  system.host = "fractal";

                  programs.editors.emacs.locals = {
                    enable = true;
                    file = ./home-manager/locals/fractal.el;
                  };
                };

              };
            };
          }
        ];
      };

      nixOnDroidConfigurations.thanatos = nix-on-droid.lib.nixOnDroidConfiguration {
        modules = [
          {
            system.stateVersion = "24.05";
            nix.extraOptions = ''
              experimental-features = nix-command flakes
            '';
          }
          {
            home-manager = {
              useGlobalPkgs = true;
              extraSpecialArgs = {
                inherit nix-path nix-neovim;
                pkgs = aarch-pkgs;
              };
              config = {
                imports = [
                  ./home-manager/modules/fonts.nix
                  ./home-manager/modules/programs/term.nix
                  ./home-manager/modules/programs/dev/nix.nix
                  ./home-manager/modules/programs/dev/tools.nix
                  ./home-manager/modules/programs/editors/helix.nix
                  ./home-manager/modules/programs/editors/neovim.nix
                ];

                home = {
                  stateVersion = "24.05";
                  packages = [
                    pkgs.openssh
                  ];

                  sessionVariables = {
                    EDITOR = "nvim";
                    NIX_PATH = nix-path;
                  };
                };
              };
            };
          }
        ];

        ## Different pkgs, need to use the nix-on-droid overlay
        pkgs = aarch-pkgs;

      };
    };
}
