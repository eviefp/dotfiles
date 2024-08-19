/****************************************************************************
  * Hardware module
  *
  * Audio, video, bluetooth, etc., settings.
  **************************************************************************/
{ lib, config, pkgs, ... }:
let cfg = config.evie.hardware.nvidia;
in
{
  options.evie.hardware.nvidia = {
    enable = lib.options.mkEnableOption "Use NVidia instead of Intel drivers.";
    useOpen = lib.options.mkEnableOption "Use NVidia open drivers.";
  };

  config = (lib.mkMerge [
    {
      services = {
        blueman.enable = true;

        pipewire = {
          enable = true;
          wireplumber.enable = true;
        };

        xserver = {
          enable = false;
          videoDrivers = if cfg.enable then [ "nvidia" ] else [ "intel" ];
          # desktopManager = {
          #   plasma5.enable = false;
          #   xterm.enable = false;
          # };
          # xkb.layout = "us";
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
            if cfg.enable
            then [ pkgs.nvidia-vaapi-driver pkgs.libglvnd pkgs.vaapiVdpau pkgs.libvdpau-va-gl ]
            else with pkgs; [ vaapiIntel libvdpau-va-gl intel-media-driver ];
          extraPackages32 =
            if cfg.enable
            then [ ]
            else with pkgs.pkgsi686Linux; [ vaapiIntel libvdpau-va-gl intel-media-driver ];
        };
      };

    }
    (lib.mkIf cfg.enable {
      hardware.nvidia = {
        modesetting.enable = true;
        open = cfg.useOpen;
        package = config.boot.kernelPackages.nvidiaPackages.latest;
        nvidiaSettings = true;
      };
    })
  ]);
}
