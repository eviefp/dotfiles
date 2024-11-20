/****************************************************************************
  * Hardware module
  *
  * Audio, video, bluetooth, etc., settings.
  * TODO: split
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.hardware;
in
{
  options.evie.hardware = {
    enable = lib.mkEnableOption "hardware";
    nvidia = {
      enable = lib.options.mkEnableOption "Use NVidia instead of Intel drivers.";
      useOpen = lib.options.mkEnableOption "Use NVidia open drivers.";
    };
    amdgpu = {
      enable = lib.options.mkEnableOption "Use AMD Radeon drivers.";
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      services = {
        blueman.enable = true;

        # TODO: rtfm
        pipewire = {
          enable = true;
          wireplumber.enable = true;
        };
      };

      # moonlander
      environment.systemPackages = [
        pkgs.wally-cli
      ];

      hardware = {
        # steam/controllers
        xone.enable = false;
        xpadneo.enable = true;
        steam-hardware.enable = true;
        # moonlander
        keyboard.zsa.enable = true;

        # pair with blueman
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
          enable = false;
        };

        graphics = {
          enable = true;
          enable32Bit = true;
          extraPackages =
            if cfg.nvidia.enable then [ pkgs.nvidia-vaapi-driver pkgs.libglvnd pkgs.vaapiVdpau pkgs.libvdpau-va-gl ]
            else if cfg.amdgpu.enable then [ pkgs.libvdpau-va-gl ]
            else with pkgs; [ vaapiIntel libvdpau-va-gl intel-media-driver ];
          extraPackages32 =
            if cfg.nvidia.enable then [ ]
            else if cfg.amdgpu.enable then with pkgs.pkgsi686Linux; [ libvdpau-va-gl ]
            else with pkgs.pkgsi686Linux; [ vaapiIntel libvdpau-va-gl intel-media-driver ];
        };
      };

    }
    (lib.mkIf cfg.nvidia.enable {
      hardware.nvidia = {
        modesetting.enable = true;
        open = cfg.nvidia.useOpen;
        package = config.boot.kernelPackages.nvidiaPackages.latest;
        nvidiaSettings = true;
      };
    })
    (lib.mkIf cfg.amdgpu.enable {
      hardware.amdgpu = {
        initrd.enable = true;
        opencl.enable = true;
        amdvlk.enable = true;
        amdvlk.support32Bit.enable = true;
        # https://github.com/GPUOpen-Drivers/AMDVLK?tab=readme-ov-file#runtime-settings
        amdvlk.settings = { };
      };
    })
  ]);
}
