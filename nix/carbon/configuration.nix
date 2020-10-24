# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "carbon"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.wireless.networks = {
    hello = {
      psk = "mamaliga666";
      priority = 100;
    };
    hello_5G = {
      psk = "mamaliga666";
      priority = 50;
    };
    ASUS = {
      psk = "jimmysnacks";
      priority = 10;
    };
  };

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp2s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Set your time zone.
  time.timeZone = "Europe/Bucharest";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    haskell.packages.ghc865.ghc
    cachix
    neovim
    haskellPackages.xmobar
    dmenu
    gmrun
    # apcilight
    pass passff-host dbus pinentry_gnome transmission-gtk pavucontrol
    konsole xorg.xmodmap xorg.xev firefox
    xdg_utils git
  ];

  fonts.fonts = [ pkgs.nerdfonts ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryFlavor = "gnome3";
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 22 80 8080 ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.hplip pkgs.gutenprint ];

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };
  hardware.video.hidpi.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    dpi = 200;
    monitorSection = ''
      Option "DPMS" "false"
    '';
    serverFlagsSection = ''
      Option "BlankTime" "0"
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
	    export XCOMPOSEFILE = /home/vlad/.XCompose
      '';
    };
    layout = "us";
    libinput.enable = true;
  };

  nix.extraOptions = ''
    binary-caches-parallel-connections = 5
  '';

  services.lorri.enable = true;

  services.udev.extraRules = ''
    KERNEL=="card0", SUBSYSTEM=="drm", ENV{XAUTHORITY}="/home/vlad/.Xauthority", RUN+="${pkgs.autorandr}/bin/autorandr --change --batch"
    ACTION=="add", SUBSYSTEM=="input", RUN+="${pkgs.xorg.setxkbmap}/bin/setxkbmap -option caps:none"
    ACTION=="add", SUBSYSTEM=="input", RUN+="${pkgs.xorg.xmodmap}/bin/xmodmap -e \"keycode 66 = Multi_key\""
  '';
  services.udev.path = [ pkgs.autorandr pkgs.xorg.xmodmap pkgs.su pkgs.coreutils pkgs.xorg.setxkbmap ];
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.vlad = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "video" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.fish;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?

}

