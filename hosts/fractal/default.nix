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
    evie = {
      common.enable = true;
      hardware = {
        enable = true;
      };
      network = {
        hostName = "fractal";
        interface = "eno1";
        extraPorts = [ 1025 1143 2049 ];
      };
      packages.extra = [ pkgs.git pkgs.wget pkgs.ntfs3g ];
    };

    services.nfs = {
      server = {
        enable = true;
        hostName = "fractal";
        exports = ''
          /mnt/raid1 192.168.10.0/24(rw,async)
        '';
      };
    };
  };
}
