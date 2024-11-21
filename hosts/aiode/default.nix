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

  # TODO: logind?
  config = {
    evie.network = {
      hostName = "aiode";
      interface = "enp0s31f6";
      wifi = {
        enable = true;
        interface = "wlp2s0";
      };
    };

    evie.wayland.compositors = [ "hyprland" "river" ];

    evie.packages.extra = [ pkgs.libva pkgs.libva-utils ];
  };
}
