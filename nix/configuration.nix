#.enable = true;c Edit this configuration file to define what should be installed on
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

  boot.initrd.network.ssh.enable = true;

  networking.hostName = "desktop"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;

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
    haskell.packages.ghc865.ghc cachix
    haskell.packages.ghc882.ghc # do I need this?
    neovim
    haskellPackages.xmobar dmenu gmrun acpilight
    pass passff-host dbus pinentry_gnome transmission-gtk libsForQt5.vlc pavucontrol
    konsole xorg.xmodmap xorg.xev
    git-radar mailutils
    python3 # tridactyl native thing
    xdg_utils
    git
  ];

  fonts.fonts = with pkgs; [
    nerdfonts
  ];

  programs.fish.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.mtr.enable = true;
  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };
  services.sshd.enable = true;

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
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };
  hardware.pulseaudio.support32Bit = true;

  services.xserver.windowManager = {
    xmonad.enable = true;
    xmonad.enableContribAndExtras = true;
  };

  hardware.opengl.driSupport32Bit = true;
  hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.desktopManager.xterm.enable = false;
  services.xserver.displayManager.defaultSession = "none+xmonad";
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.sessionCommands = ''
    setxkbmap -option caps:none
    xmodmap -e "keycode 66 = Multi_key"
    export XCOMPOSEFILE = /home/vlad/.XCompose
  '';
  services.xserver.videoDrivers = ["nvidia"];
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "eurosign:e";
  # services.compton.enable = true;

  # Enable touchpad support.
  services.xserver.libinput.enable = true;

  # Enable the KDE Desktop Environment.
  services.xserver.displayManager.sddm.enable = false;
  services.xserver.desktopManager.plasma5.enable = false;

  # ${pkgs.autorandr}/bin/autorandr --change --batch"
  #  SUBSYSTEM=="usb", ID_VENDOR="Razer", RUN+="${pkgs.coreutils}/bin/touch /home/vlad/test2"
  #  SUBSYSTEM=="usb", ID_VENDOR="Razer", RUN+="${pkgs.coreutils}/bin/echo $USER > /home/vlad/test"
  # SUBSYSTEM=="usb", ID_VENDOR="Razer", RUN+="${pkgs.xorg.setxkbmap}/bin/setxkbmap -option caps:none"
  # SUBSYSTEM=="usb", ID_VENDOR="Razer", RUN+="${pkgs.xorg.xmodmap}/bin/xmodmap -e 'keycode 66 = Multi_key'"
  services.udev.extraRules = ''

    KERNEL=="card0", SUBSYSTEM=="drm", ENV{XAUTHORITY}="/home/vlad/.Xauthority", RUN+="${pkgs.autorandr}/bin/autorandr --change --batch"
    ACTION=="add", SUBSYSTEM=="input", RUN+="${pkgs.xorg.setxkbmap}/bin/setxkbmap -option caps:none"
    ACTION=="add", SUBSYSTEM=="input", RUN+="${pkgs.xorg.xmodmap}/bin/xmodmap -e \"keycode 66 = Multi_key\""
  '';
  services.udev.path = [ pkgs.autorandr pkgs.xorg.xmodmap pkgs.su pkgs.coreutils pkgs.xorg.setxkbmap ];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.vlad = {
    isNormalUser = true;
    extraGroups =
      [ "wheel"            # enable sudo
        "networkmanager"   # control wi-fi
        "video"            # control backlight
      ];
    shell = pkgs.fish;
  };
  # users.users.root.initialHashedPassword = "";

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.03"; # Did you read the comment?

  networking.firewall.allowedTCPPorts = [ 22 80 8080 ];
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

  nix.binaryCaches = [
    "https://cache.nixos.org"
    "https://kore.cachix.org"
  ];

  nix.extraOptions = ''
    binary-caches-parallel-connections = 5
  '';

  services.lorri.enable = true;
}

