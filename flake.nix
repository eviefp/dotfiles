{
  inputs = {
    lix-module = {
      url = "https://git.lix.systems/lix-project/nixos-module/archive/2.90.0-rc1.tar.gz";
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
    { lix-module, nixpkgs, home-manager, nix-on-droid, nix-neovim, emacs-overlay, nil, porc, gitu, lean4-mode, hyprland, hyprpaper, hyprpicker, hypridle, hyprlock, hyprcursor, ect, sops-nix, ... }:
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
      nixosConfigurations."thelxinoe" = nixpkgs.lib.nixosSystem {
        system = system;
        specialArgs = { inherit hyprland; };
        modules = [
          lix-module.nixosModules.default
          ./system/modules/common.nix
          ./system/modules/peroxide.nix
          ./system/modules/xserver.nix
          ./system/hardware/thelxinoe.nix
          {
            evie.network = {
              hostName = "thelxinoe";
              interface = "enp4s0";
              extraPorts = [ 31234 ];
            };
            evie.xserver.useNVidia = true;
            evie.services.peroxide = {
              enable = true;
              certificate-name = "thelxinoe";
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
                  ./home-manager/modules/programs/streaming.nix
                  ./home-manager/modules/email.nix
                  ./home-manager/modules/programs/dev.nix
                ];

                home.packages = [
                  pkgs.nvtopPackages.full
                ];


                evie = {
                  system.host = "thelxinoe";

                  programs.editors.emacs.locals = {
                    enable = true;
                    file = ./home-manager/locals/thelxinoe.el;
                  };

                  wayland = {
                    eww-monitor = "0";
                    showTV = true;
                    useSshMailCalendar = false;
                    showMail = true;
                    showCalendar = true;
                    monitors = [
                      {
                        name = "DP-1";
                        resolution = "1920x1080@239.76";
                        position = "0x0";
                        keybind = "W";
                      }
                      {
                        name = "DP-3";
                        resolution = "1920x1080@239.76";
                        position = "1920x0";
                        keybind = "E";
                      }
                      {
                        name = "DP-2";
                        resolution = "1920x1080@239.76";
                        position = "3840x0";
                        keybind = "R";
                      }
                      {
                        name = "HDMI-A-2";
                        resolution = "1920x1080@60";
                        position = "5760x0";
                        keybind = "T";
                      }
                    ];
                    disabledMonitors = [ "Unknown-1" ];
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
                  ./home-manager/modules/programs/streaming.nix
                  # ./home-manager/modules/email.nix
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
                  ./home-manager/modules/programs/streaming.nix
                  # ./home-manager/modules/email.nix
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
            networking.firewall.allowedUDPPorts = [ ];
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
