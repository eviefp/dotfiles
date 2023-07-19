/****************************************************************************
  * XServer module
  *
  * XServer, audio, and video settings.
  **************************************************************************/
{ lib, config, pkgs, ... }:
let cfg = config.evie.xserver;
in
{
  imports = [ ];

  options.evie.xserver = {
    enable = lib.options.mkEnableOption "Enable XServer.";
    useNVidia =
      lib.options.mkEnableOption "Use NVidia instead of Intel drivers.";
    useBluetooth = lib.options.mkEnableOption "Enable bluetooth support.";
    monitorSectionDisplaySize = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = "The DisplaySize monitor section, e.g. 'DisplaySize 100 100'";
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      services = {
        xserver = {
          enable = true;
          videoDrivers = if cfg.useNVidia then [ "nvidia" ] else [ "intel" ];
          monitorSection = ''
            Option "DPMS" "false"
            ${cfg.monitorSectionDisplaySize}
          '';
          serverFlagsSection = ''
            Option "BlankTime" "20"
          '';
          windowManager.xmonad = {
            enable = true;
            enableContribAndExtras = true;
          };
          desktopManager = {
            plasma5.enable = false;
            xterm.enable = false;
          };
          displayManager = {
            defaultSession = "none+xmonad";
            gdm.enable = true;
            sessionCommands = ''
              setxkbmap -option caps:none
              xmodmap -e "keycode 66 = Multi_key"
              export XCOMPOSEFILE = /home/evie/.XCompose
            '';
          };
          layout = "us";
          libinput.enable = true;
        };
        picom = {
          enable = true;
          fade = true;
        };
      };

      hardware = {
        cpu.intel.updateMicrocode = true;
        pulseaudio = {
          enable = true;
          package = pkgs.pulseaudioFull;
        };
        opengl = {
          enable = true;
          driSupport32Bit = true;
          driSupport = true;
          extraPackages =
            if cfg.useNVidia
            then [ pkgs.libglvnd pkgs.vaapiVdpau pkgs.libvdpau-va-gl ]
            else with pkgs; [ vaapiIntel libvdpau-va-gl intel-media-driver ];
          extraPackages32 =
            if cfg.useNVidia
            then [ ]
            else with pkgs.pkgsi686Linux; [ vaapiIntel libvdpau-va-gl intel-media-driver ];
        };
      };

      sound.enable = true;
    }
    (lib.mkIf cfg.useBluetooth {
      services.blueman.enable = true;
      hardware.bluetooth.enable = true;
    })
  ]);
}
