{ dotfiles, ... }:
{
  imports = [
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
    nixpkgs.overlays = [ (import dotfiles.nix-on-droid.overlays.default) ];
    nixpkgs.config.packageOverrides = pkgs: {
      vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
    };

    # Randomly decided the NixOS version should be here.
    system.stateVersion = "24.11";
  };
}
