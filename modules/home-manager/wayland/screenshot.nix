{ pkgs, ... }:
{
  config.home.packages = [
    pkgs.grimblast
    pkgs.wl-clipboard
  ];
}
