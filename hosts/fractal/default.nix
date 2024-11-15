{ dotfiles, pkgs, ... }:
{
  imports = with dotfiles.self.nixosModules; [
    dotfiles.lix-module.nixosModules.default
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
    evie.network = {
      hostName = "fractal";
      interface = "eno1";
      extraPorts = [ 1025 1143 2049 ];
    };
    evie.packages.extra = [ pkgs.git pkgs.wget ];

    services.nfs = {
      server = {
        enable = true;
        hostName = "fractal";
        exports = ''
          /mnt/raid1 192.168.10.0/24(rw,async)
        '';
      };
    };

    # Randomly decided the NixOS version should be here.
    system.stateVersion = "24.11";
  };
}
