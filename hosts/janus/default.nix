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

  config = {
    nixpkgs.config.packageOverrides = pkgs: {
      vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
    };

    evie = {
      common.enable = true;
      hardware.enable = true;
      network = {
        enable = true;
        hostName = "janus";
        enableWifi = true;
      };
      packages.extra = [ pkgs.libva pkgs.libva-utils ];
      wayland = {
        enable = true;
        compositors = [ "hyprland" ];
      };
    };

  };
}
