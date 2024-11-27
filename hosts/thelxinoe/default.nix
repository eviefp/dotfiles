{ dotfiles, ... }:
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

  config.evie = {
    common.enable = true;
    hardware = {
      bluetooth.enable = true;
      moonlander.enable = true;
      pipewire.enable = true;
      video = {
        enable = true;
        amdgpu.enable = true;
      };
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
}
