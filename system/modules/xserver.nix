/****************************************************************************
  * XServer module
  *
  * XServer, audio, and video settings.
  **************************************************************************/
{ lib, config, pkgs, hyprland, ... }:
let cfg = config.evie.xserver;
in
{
  imports = [ ];

  options.evie.xserver = {
    useNVidia =
      lib.options.mkEnableOption "Use NVidia instead of Intel drivers.";
    monitorSectionDisplaySize = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = "The DisplaySize monitor section, e.g. 'DisplaySize 100 100'";
    };
  };

  config = (lib.mkMerge [
    {
      programs.hyprland = {
        enable = true;
        package = hyprland.packages.${pkgs.system}.hyprland;
        xwayland.enable = true;
      };

      services = {
        blueman.enable = true;
        pipewire = {
          enable = true;
          wireplumber.enable = true;
        };
        xserver = {
          enable = true;
          videoDrivers = if cfg.useNVidia then [ "nvidia" ] else [ "intel" ];
          monitorSection = ''
            Option "DPMS" "false"
          '';
          windowManager.xmonad = {
            enable = false;
            enableContribAndExtras = true;
            config = ../../config/xmonad/xmonad.hs;
            haskellPackages = pkgs.haskell.packages.ghc946;
            extraPackages = haskellPackages: [
              haskellPackages.async
              haskellPackages.scotty
              haskellPackages.raw-strings-qq
            ];
            ghcArgs = [
              "-threaded"
            ];
            xmonadCliArgs = [
              "+RTS"
              "-N4"
              "-RTS"
            ];
          };
          desktopManager = {
            plasma5.enable = false;
            xterm.enable = false;
          };
          xkb.layout = "us";
        };

        displayManager.sddm = {
          enable = true;
          theme = "/run/current-system/sw/share/sddm/themes/elarun";
          wayland.enable = true;
        };
      };

      hardware = {
        cpu.intel.updateMicrocode = true;
        bluetooth.enable = true;
        pulseaudio = {
          enable = true;
          package = pkgs.pulseaudioFull;
        };
        nvidia = {
          modesetting.enable = true;
          open = true;
          package = config.boot.kernelPackages.nvidiaPackages.latest;
        };
        opengl = {
          enable = true;
          driSupport32Bit = true;
          driSupport = true;
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

      sound.enable = true;

      environment.variables = {
        XDG_SESSION_TYPE = "wayland";
      };
    }
    (lib.mkIf cfg.useNVidia {
      environment.variables = {
        WLR_DRM_NO_ATOMIC = "1";
        LIBVA_DRIVER_NAME = "nvidia";
        GBM_BACKEND = "nvidia-drm";
        NVD_BACKEND = "direct";
        __GLX_VENDOR_LIBRARY_NAME = "nvidia";
        __GLX_VRR_ALLOWED = "1";
      };
    })
    (lib.mkIf (!cfg.useNVidia) {
      environment.variables = {
        VPAU_DRIVER = "va_gl";
      };
    })
  ]);
}
