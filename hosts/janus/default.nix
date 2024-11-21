{ dotfiles, pkgs, ... }:
{
  imports = with dotfiles.self.nixosModules; [
    dotfiles.lix-module.nixosModules.default

    common
    hardware
    wayland

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

    evie.network = {
      hostName = "janus";
      wifi.enable = true;
    };

    evie.wayland.compositors = [ "hyprland" "river" ];

    evie.packages.extra = [ pkgs.libva pkgs.libva-utils ];
  };
}
