{
  boot = ./boot.nix;
  common = ./common.nix;
  hardware = import ./hardware;
  jellyfin = ./jellyfin.nix;
  locale = ./locale.nix;
  logind = ./logind.nix;
  minecraft = ./minecraft.nix;
  network = ./network.nix;
  nextcloud = ./nextcloud.nix;
  nix-settings = ./nix-settings.nix;
  packages = ./packages.nix;
  peroxide = ./peroxide.nix;
  services = ./services.nix;
  sops = ./sops.nix;
  users = ./users.nix;
  wayland = ./wayland.nix;
  yubikey = ./yubikey.nix;
}
