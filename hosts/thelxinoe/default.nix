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
    evie.network = {
      hostName = "thelxinoe";
      interface = "enp4s0";
      extraPorts = [ 31234 ];
    };

    evie.common.enable = true;

    evie.hardware = {
      enable = true;
      amdgpu.enable = true;
    };

    evie.wayland.compositors = [ "hyprland" ];
    evie.services.peroxide = {
      enable = true;
      package = pkgs.callPackage dotfiles.self.nixosModules.peroxide-override { };
    };

    # Randomly decided the NixOS version should be here.
    system.stateVersion = "25.05";
  };
}
