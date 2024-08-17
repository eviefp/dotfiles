/****************************************************************************
  * XServer module
  *
  * XServer, audio, and video settings.
  * TODO: rename to hardware or something
  **************************************************************************/
{ lib, config, pkgs, dotfiles, ... }:
let cfg = config.evie.xserver;
in
{
  options.evie.xserver = {
    useNVidia =
      lib.options.mkEnableOption "Use NVidia instead of Intel drivers.";
  };

  config = (lib.mkMerge [
    {
      # TODO: make this configurable
      programs.hyprland = {
        enable = true;
        package = dotfiles.hyprland.packages.${pkgs.system}.hyprland;
        xwayland.enable = true;
      };

      services = {
        blueman.enable = true;

        pipewire = {
          enable = true;
          wireplumber.enable = true;
        };

        xserver = {
          enable = false;
          videoDrivers = if cfg.useNVidia then [ "nvidia" ] else [ "intel" ];
          # desktopManager = {
          #   plasma5.enable = false;
          #   xterm.enable = false;
          # };
          # xkb.layout = "us";
        };

        # TODO: try regreet
        displayManager.sddm = {
          enable = true;
          theme = "/run/current-system/sw/share/sddm/themes/elarun";
          wayland.enable = true;
        };
      };

      environment.systemPackages = [
        pkgs.wally-cli
      ];

      hardware = {
        xone.enable = false;
        xpadneo.enable = true;
        steam-hardware.enable = true;
        keyboard.zsa.enable = true;

        bluetooth = {
          enable = true;
          settings = {
            General = {
              ControllerMode = "dual";
              # Privacy = "???";
            };
            Policy = {
              AutoEnable = true;
            };
          };
        };

        pulseaudio = {
          enable = true;
          package = pkgs.pulseaudioFull;
        };

        graphics = {
          enable = true;
          enable32Bit = true;
          extraPackages =
            if cfg.useNVidia
            then [ pkgs.nvidia-vaapi-driver pkgs.libglvnd pkgs.vaapiVdpau pkgs.libvdpau-va-gl ]
            else with pkgs; [ vaapiIntel libvdpau-va-gl intel-media-driver ];
          extraPackages32 =
            if cfg.useNVidia
            then [ ]
            else with pkgs.pkgsi686Linux; [ vaapiIntel libvdpau-va-gl intel-media-driver ];
        };
      };

    }
    (lib.mkIf cfg.useNVidia {
      nixpkgs.config.allowUnfree = true;
      hardware.nvidia = {
        modesetting.enable = true;
        open = true;
        package = config.boot.kernelPackages.nvidiaPackages.latest;
        nvidiaSettings = true;
      };
      environment.variables = {
        # WLR_DRM_NO_ATOMIC = "1";
        # __GLX_VRR_ALLOWED = "1";
      };
    })
    (lib.mkIf (!cfg.useNVidia) {
      environment.variables = {
        # VPAU_DRIVER = "va_gl";
      };
    })
  ]);
}
