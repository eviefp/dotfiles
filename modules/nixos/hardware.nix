/****************************************************************************
  * Hardware module
  *
  * Audio, video, bluetooth, etc., settings.
  **************************************************************************/
{ lib, config, pkgs, ... }:
let cfg = config.evie.hardware;
in
{
  options.evie.hardware.nvidia = {
    enable = lib.options.mkEnableOption "Use NVidia instead of Intel drivers.";
    useOpen = lib.options.mkEnableOption "Use NVidia open drivers.";
  };
  options.evie.hardware.amdgpu = {
    enable = lib.options.mkEnableOption "Use AMD Radeon drivers.";
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
          videoDrivers =
            if cfg.nvidia.enable then [ "nvidia" ]
            else if cfg.amdgpu.enable then [ "amdgpu" ]
            else [ "intel" ];
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
        # amdvlk.settings = {}; TODO
      };
    })
  ]);
}
