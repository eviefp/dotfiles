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

  config = {
    nixpkgs.config.packageOverrides = pkgs: {
      vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
    };

    evie = {
      common.enable = true;
      hardware = {
        bluetooth.enable = true;
        pipewire.enable = true;
        video.enable = true;
      };
      network = {
        enable = true;
        enableWifi = true;
        hostName = "janus";
      };
      packages.extra = [ pkgs.libva pkgs.libva-utils ];
      wayland = {
        enable = true;
        compositors = [ "hyprland" ];
      };
    };

  };
}
