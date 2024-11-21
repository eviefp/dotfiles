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

    evie = {
      common.enable = true;
      hardware = {
        enable = true;
        amdgpu.enable = true;
      };
      network = {
        enable = true;
        hostName = "thelxinoe";
        extraPorts = [ 31234 ];
      };
      peroxide.enable = true;
    };

    evie.wayland.compositors = [ "hyprland" ];
  };
}
