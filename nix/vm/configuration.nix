# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

	{ config, pkgs, ... }:

	{
	  imports =
	    [ # Include the results of the hardware scan.
	      ./hardware-configuration.nix
	    ];

	  # Use the GRUB 2 boot loader.
	  boot.loader.grub.enable = true;
	  boot.loader.grub.version = 2;
	  # boot.loader.grub.efiSupport = true;
	  # boot.loader.grub.efiInstallAsRemovable = true;
	  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
	  # Define on which hard drive you want to install Grub.
	  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

	  networking.hostName = "vm"; # Define your hostname.
	  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

	  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
	  # Per-interface useDHCP will be mandatory in the future, so this generated config
	  # replicates the default behaviour.
	  networking.useDHCP = false;
	  networking.interfaces.enp0s3.useDHCP = true;

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
	    haskell.packages.ghc882.ghc
	    neovim
	    haskellPackages.xmobar dmenu gmrun
            # todo: for passff:
            # ln -s /nix/store/5k89g8p9jrqaks6895sj7xbjk4k975si-passff-host-1.2.1/lib/mozilla/native-messaging-hosts/passff.json ~/.mozilla/native-messaging-hosts/passff.json
	    pass passff-host dbus pinentry_gnome transmission-gtk libsForQt5.vlc pavucontrol
	    xorg.xmodmap xorg.xev
	    git-radar mailutils
	    python3
	    xdg_utils
	    git
	    linuxPackages.virtualbox
	    linuxPackages.virtualboxGuestAdditions
	  ];

	  fonts.fonts = [ pkgs.nerdfonts ];

	  programs.fish.enable = true;
	  programs.mtr.enable = true;
	  programs.gnupg.agent = {
	    enable = true;
	    enableSSHSupport = true;
	  };

	  # Enable the OpenSSH daemon.
	  services.openssh.enable = true;

	  # Open ports in the firewall.
	  # networking.firewall.allowedTCPPorts = [ ... ];
	  # networking.firewall.allowedUDPPorts = [ ... ];
	  # Or disable the firewall altogether.
	  # networking.firewall.enable = false;

	  # Enable CUPS to print documents.
	  # services.printing.enable = true;

	  # Enable sound.
	  sound.enable = true;
	  hardware.pulseaudio = {
	    enable = true;
	    package = pkgs.pulseaudioFull;
	  };
	  hardware.pulseaudio.support32Bit = true;
	  # hardware.opengl.driSupport32Bit = true;
	  # hardware.opengl.extraPackages32 = [ pkgs.pkgsi686Linux.libva ];

	  services.xserver.windowManager = {
	    xmonad.enable = true;
	    xmonad.enableContribAndExtras = true;
	  };

	  # Enable the X11 windowing system.
	  services.xserver = {
	    enable = true;
	    monitorSection = ''
	      Option "DPMS" "false"
	    '';
	    serverFlagsSection = ''
	      Option "BlankTime" "0"
	    '';
	  };
	  services.xserver.desktopManager.xterm.enable = false;
	  services.xserver.displayManager.defaultSession = "none+xmonad";
	  services.xserver.displayManager.lightdm.enable = true;
	  services.xserver.displayManager.sessionCommands = ''
	    setxkbmap -option caps:none
	    xmodmap -e "keycode 66 = Multi_key"
	    export XCOMPOSEFILE = /home/vlad/.XCompose
	  '';
	  #services.xserver.videoDrivers = [ "vboxvideo" "vesa" ];
	  # services.xserver.videoDriver = "vesa";
	  services.xserver.layout = "us";
	  services.xserver.xkbOptions = "eurosign:e";
	  # services.compton.enable = true;

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
	    shell = pkgs.fish;
	  };

	  # users.users.root.initialHashedPassword = "";

	  # This value determines the NixOS release with which your system is to be
	  # compatible, in order to avoid breaking some software such as database
	  # servers. You should change this only after NixOS release notes say you
	  # should.

	  nix.extraOptions = ''
	    binary-caches-parallel-connections = 5
	  '';

	  services.lorri.enable = true;

	  system.stateVersion = "20.03"; # Did you read the comment?
	}

