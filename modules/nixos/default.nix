{
  common = ./common.nix;
  boot = ./boot.nix;
  hardware = ./hardware.nix;
  locale = ./locale.nix;
  network = ./network.nix;
  nix-settings = ./nix-settings.nix;
  peroxide = ./peroxide.nix;
  peroxide-override = ./peroxide-override.nix;
  packages = ./packages.nix;
  services = ./services.nix;
  sops = ./sops.nix;
  users = ./users.nix;
  wayland = ./wayland.nix;
}
