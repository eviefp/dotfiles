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
        users.evie = ./home.nix;
      };
    }
  ];

  config.evie = {
    common.enable = true;
    hardware.enable = true;
    network = {
      enable = true;
      hostName = "aiode";
    };
    packages.extra = [ pkgs.libva pkgs.libva-utils ];
    wayland = {
      enable = true;
      compositors = [ "hyprland" ];
    };
    yubikey.enable = true;
  };
}
