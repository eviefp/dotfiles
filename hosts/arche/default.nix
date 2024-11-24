{ dotfiles, pkgs, ... }:
{
  imports = with dotfiles.self.nixosModules; [
    common

    ./hardware.nix

    dotfiles.home-manager.nixosModules.home-manager
    {
      home-manager = {
        backupFileExtension = "backup";
        useGlobalPkgs = true;
        useUserPackages = true;
        extraSpecialArgs = { inherit dotfiles; };
        users.every = ./home.nix;
        users.evie = ./home-evie.nix;
      };
    }
  ];

  config = {
    users.users.every = {
      isNormalUser = true;
      extraGroups = [
        "wheel"
        "networkmanager"
        "video"
        "docker"
        "plugdev"
        "vboxusers"
        "input"
        "uinput"
        "lp"
      ];
      shell = pkgs.fish;
      hashedPassword =
        "$6$2bJFtErxPXqeCEJO$w4K0Fm1WmRL3tpUUJxkesiBFsM03Q2/IrtX9QvJjIBH3bxlOr1VtMIgWhCtIR1B./3QtmBCKo4H8ajTk51JW2/";
    };

    evie = {
      common.enable = true;
      hardware = {
        enable = true;
        nvidia = {
          enable = true;
          useOpen = false;
        };
      };
      network = {
        hostName = "arche";
      };

      wayland.compositors = [ "hyprland" "plasma" ];
    };


  };
}
