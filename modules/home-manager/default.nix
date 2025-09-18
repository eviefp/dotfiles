{
  common = ./common.nix;
  dev = import ./dev;
  editors = import ./editors;
  macos = import ./macos;
  programs = import ./programs;
  system = import ./system;
  term = import ./term;
  wayland = import ./wayland;
}
