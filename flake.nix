{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    neuron.url = "github:srid/neuron/master";
    emacs-overlay.url = "github:nix-community/emacs-overlay/master";
  };

  outputs = { self, nixpkgs, home-manager, unstable, neuron, emacs-overlay }: {

    nixosConfigurations."thelxinoe" = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules =
        [
          ./system/thelxinoe/configuration.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.evie = import ./home-manager/thelxinoe/home.nix {
              unstable = unstable;
              neuron = neuron;
              emacs-overlay = emacs-overlay;
            };
          }
        ];
    };

  };
}
