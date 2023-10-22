{ config, lib, pkgs, ... }:
{
  system.stateVersion = "23.11";

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

}
