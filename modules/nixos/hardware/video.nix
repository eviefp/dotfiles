/****************************************************************************
  * Video
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.hardware.video;
in
{
  options.evie.hardware.video = {
    enable = lib.mkEnableOption "pipewire";
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
      hardware.graphics = {
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
    }
    (lib.mkIf cfg.nvidia.enable {
      hardware.nvidia = {
        modesetting.enable = true;
        open = cfg.nvidia.useOpen;
        package = config.boot.kernelPackages.nvidiaPackages.stable;
        nvidiaSettings = true;
      };
    })
    (lib.mkIf cfg.amdgpu.enable {
      environment.systemPackages = [ pkgs.lact ];
      hardware.amdgpu = {
        initrd.enable = true;
        overdrive.enable = true;
        opencl.enable = true;
        amdvlk.enable = true;
        amdvlk.support32Bit.enable = true;
        # https://github.com/GPUOpen-Drivers/AMDVLK?tab=readme-ov-file#runtime-settings
        amdvlk.settings = { };
      };
    })
  ]);
}
