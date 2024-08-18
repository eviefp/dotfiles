/****************************************************************************
  * Hardware module
  *
  * Audio, video, bluetooth, etc., settings.
  **************************************************************************/
{ lib, config, pkgs, ... }:
let cfg = config.evie.hardware;
in
{
  options.evie.hardware = {
    useNVidia =
      lib.options.mkEnableOption "Use NVidia instead of Intel drivers.";
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
          videoDrivers = if cfg.useNVidia then [ "nvidia" ] else [ "intel" ];
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
    })
  ]);
}
