{ config, pkgs, ... }:

{
  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  networking = {
    useDHCP = false;
    firewall.allowedTCPPorts = [ 22 80 443 8080 ];
    hosts = {
      "192.168.1.1" = [ "router" ];
      "192.168.1.15" = [ "bridge" ];
      "192.168.10.177" = [ "thelxinoe" ];
      "192.168.10.206" = [ "fractal" ];
      "192.168.10.1" = [ "router2" ];
      "192.168.10.67" = [ "arche" ];
      "192.168.10.25" = [ "aiode" ];
    };
  };

  i18n.defaultLocale = "en_US.UTF-8";

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  time.timeZone = "Europe/Bucharest";

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    # haskell.packages.ghc8107.ghc
    cachix
    dbus
    vim
    xorg.xmodmap
  ];

  fonts.fonts = [ pkgs.nerdfonts ];

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "gnome3";
    };

    dconf.enable = true;
  };

  services = {
    openssh.enable = true;

    printing = {
      enable = true;
      drivers = [ pkgs.hplip pkgs.gutenprint ];
    };

    lorri.enable = true;

    udev = {
      extraRules = ''
SUBSYSTEM=="usb", ATTR{idVendor}=="3297", GROUP="plugdev"
ACTION=="add", SUBSYSTEM=="input", RUN+="${pkgs.xorg.setxkbmap}/bin/setxkbmap -option caps:none"
ACTION=="add", SUBSYSTEM=="input", RUN+="${pkgs.xorg.xmodmap}/bin/xmodmap -e \"keycode 66 = Multi_key\""
'';
      path = [ pkgs.autorandr pkgs.xorg.xmodmap pkgs.su pkgs.coreutils pkgs.xorg.setxkbmap ];
    };
  };

  sound.enable = true;

  hardware = {
    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
    };
    opengl = {
      enable = true;
      driSupport32Bit = true;
    };
  };


  services.xserver = {
    enable = true;
    videoDrivers = [ "nvidia" ];
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

  nix.extraOptions = ''
    binary-caches-parallel-connections = 5
  '';

  users = {
    groups = {
      plugdev = { };
    };

    users = {
      vlad = {
        isNormalUser = true;
        extraGroups = [ "wheel" "networkmanager" "video" "docker" ];
        shell = pkgs.fish;
      };

      evie = {
        isNormalUser = true;
        extraGroups = [ "wheel" "networkmanager" "video" "docker" "plugdev" ];
        shell = pkgs.fish;
      };
    };
  };

  security.sudo.wheelNeedsPassword = false;

  virtualisation.docker = {
    enable = true;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
}

