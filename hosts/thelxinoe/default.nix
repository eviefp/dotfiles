{ dotfiles, pkgs, ... }:
{
  imports = with dotfiles.self.nixosModules; [
    dotfiles.lix-module.nixosModules.default

    common
    peroxide
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

    evie.common.enable = true;
    evie.hardware = {
      enable = true;
      amdgpu.enable = true;
    };

    evie.network = {
      enable = true;
      hostName = "thelxinoe";
      extraPorts = [ 31234 ];
    };

    evie.wayland.compositors = [ "hyprland" ];
    evie.services.peroxide = {
      enable = true;
      package = pkgs.callPackage dotfiles.self.nixosModules.peroxide-override { };
    };
  };
}
