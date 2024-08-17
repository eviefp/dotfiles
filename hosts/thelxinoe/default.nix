{ dotfiles, ... }:
{
  imports = with dotfiles.self.nixosModules; [
    dotfiles.lix-module.nixosModules.default

    common
    peroxide
    xserver

    ./hardware.nix

    dotfiles.home-manager.nixosModules.home-manager
    {
      home-manager = {
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

    evie.xserver.useNVidia = true;
    evie.services.peroxide.enable = true;

    # Randomly decided the NixOS version should be here.
    system.stateVersion = "24.11";
  };
}
