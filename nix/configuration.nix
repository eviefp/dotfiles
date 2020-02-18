# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./cachix.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nixos"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

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
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Bucharest";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    haskell.packages.ghc865.ghc
    # (import (builtins.fetchTarball "https://github.com/nixos/nixpkgs/tarball/nixpkgs-unstable") {}).pkgs.neovim
    # ^ is broken so we're pinning to the last known good commit:
    (import (builtins.fetchTarball "https://github.com/nixos/nixpkgs/tarball/c8718e29b3740b9094aee842e7b157872d98942e") {}).pkgs.neovim
    haskellPackages.xmobar dmenu gmrun acpilight
    pass passff-host dbus pinentry_gnome transmission-gtk libsForQt5.vlc pavucontrol
    konsole xorg.xmodmap xorg.xev
    git-radar mailutils
    python3 # tridactyl native thing
    xdg_utils
  ];

  fonts.fonts = with pkgs; [
    nerdfonts
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.mtr.enable = true;
  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.hplip pkgs.gutenprint ];

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.acpilight.enable = true;

  services.xserver.windowManager = {
    xmonad.enable = true;
    xmonad.enableContribAndExtras = true;
    default = "xmonad";
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.desktopManager.default = "none";
  services.xserver.desktopManager.xterm.enable = false;
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.sessionCommands = ''
    setxkbmap -option caps:none
    xmodmap -e "keycode 66 = Multi_key"
    export XCOMPOSEFILE = /home/vlad/.XCompose
  '';
  services.xserver.videoDrivers = ["intel"];
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "eurosign:e";
  services.compton.enable = true;

  # Enable touchpad support.
  services.xserver.libinput.enable = true;

  # Enable the KDE Desktop Environment.
  services.xserver.displayManager.sddm.enable = false;
  services.xserver.desktopManager.plasma5.enable = false;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.vlad = {
    isNormalUser = true;
    extraGroups =
      [ "wheel"            # enable sudo
        "networkmanager"   # control wi-fi
        "video"            # control backlight
      ];
  };
  users.users.root.initialHashedPassword = "";

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?

  networking.firewall.allowedTCPPorts = [ 80 8080 ];
  # services.nginx = {
  #   enable = true;
  #   virtualHosts."localhost" = {
  #     locations."/" = {
  #       proxyPass = "http://127.0.0.1:8080";
  #       extraConfig = ''
  #         proxy_intercept_errors on;
  #         error_page 404 /404.html;
  #         error_page 500 /500.html;
  #         error_page 502 503 /maintenance.html;
  #       '';
  #     };
  #     locations."/maintenance.html" = {
  #       root = "/home/vlad/code/firefly/web/public_content";
  #     };
  #     extraConfig = ''
  #       access_log off;
  #     '';
  #   };
  # };
}

