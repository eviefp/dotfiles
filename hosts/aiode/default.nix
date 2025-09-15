{ dotfiles, pkgs, theme, ... }:
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
        extraSpecialArgs = { inherit dotfiles theme; };
        users.evie = ./home.nix;
      };
    }
  ];

  config.evie = {
    common.enable = true;
    hardware = {
      bluetooth.enable = true;
      moonlander.enable = true;
      pipewire.enable = true;
      video.enable = true;
    };
    network = {
      enable = true;
      enableWifi = true;
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
