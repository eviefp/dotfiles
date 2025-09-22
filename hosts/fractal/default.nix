{
  dotfiles,
  pkgs,
  theme,
  ...
}: {
  imports = with dotfiles.self.nixosModules; [
    common

    ./hardware.nix

    dotfiles.home-manager.nixosModules.home-manager
    {
      home-manager = {
        backupFileExtension = "backup";
        useGlobalPkgs = true;
        useUserPackages = true;
        extraSpecialArgs = {inherit dotfiles theme;};
        users.evie = ./home.nix;
      };
    }
  ];

  config = {
    networking.firewall.allowedUDPPorts = [111 2049 4000 4001 4002];
    evie = {
      common.enable = true;
      minecraft.enable = true;
      network = {
        hostName = "fractal";
        extraPorts = [111 1025 1143 2049 4000 4001 4002];
      };
      packages.extra = [pkgs.git pkgs.wget pkgs.ntfs3g];
    };

    services.nfs = {
      server = {
        enable = true;
        statdPort = 4000;
        lockdPort = 4001;
        mountdPort = 4002;
        hostName = "fractal";
        exports = ''
          /mnt/raid1 192.168.10.0/24(rw,async)
        '';
      };
    };
  };
}
