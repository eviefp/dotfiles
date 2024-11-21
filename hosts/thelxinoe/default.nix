{ dotfiles, pkgs, ... }:
{
  imports = with dotfiles.self.nixosModules; [
    common
    hardware
    peroxide
    wayland
    yubikey

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
      wayland = {
        enable = true;
        compositors = [ "hyprland" ];
      };
      yubikey.enable = true;
    };
  };
}
