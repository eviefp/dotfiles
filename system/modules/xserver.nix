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
    enableHiDPI = lib.options.mkEnableOption "Enable HiDPI support.";
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      services = {
        xserver = {
          enable = true;
          videoDrivers = if cfg.useNVidia then [ "nvidia" ] else [ "intel" ];
          monitorSection = ''
            Option "DPMS" "false"
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
            lightdm.enable = true;
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
        pulseaudio = {
          enable = true;
          package = pkgs.pulseaudioFull;
        };
        opengl = {
          enable = true;
          driSupport32Bit = true;
        };
        video.hidpi.enable = cfg.enableHiDPI;
      };

      sound.enable = true;
    }
    (lib.mkIf cfg.useBluetooth {
      services.blueman.enable = true;
      hardware.pulseaudio.extraModules = [ pkgs.pulseaudio-modules-bt ];
      hardware.bluetooth.enable = true;
    })
  ]);
}
