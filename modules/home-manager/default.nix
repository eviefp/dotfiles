{
  bower = ./programs/bower.nix;
  calendar = ./calendar.nix;
  common = ./common.nix;
  ect = ./programs/ect.nix;
  editors = {
    emacs = ./programs/editors/emacs.nix;
    neovim = ./programs/editors/neovim.nix;
    helix = ./programs/editors/helix.nix;
  };
  email = ./email.nix;
  fonts = ./fonts.nix;
  gpg = ./gpg.nix;
  gui = ./gui.nix;
  programs = {
    browsers = ./programs/browsers.nix;
    chat = ./programs/chat.nix;
    dev = {
      default = ./programs/dev.nix;
      haskell = ./programs/dev/haskell.nix;
      lua = ./programs/dev/lua.nix;
      nix = ./programs/dev/nix.nix;
      provers = ./programs/dev/provers.nix;
      tools = ./programs/dev/tools.nix;
    };
    kitty = ./programs/kitty.nix;
    ranger = ./programs/shell/ranger.nix;
    scripts = ./programs/scripts.nix;
    spotify-player = ./programs/spotify-player.nix;
    streaming = ./programs/streaming.nix;
    term = ./programs/term.nix;
    text = ./programs/text.nix;
  };
  sops = ./sops.nix;
  system = ./system.nix;
  wayland = {
    default = ./wayland.nix;
    hyprland = {
      hyprland = ./wayland/hyprland/hyprland.nix;
      hyprpaper = ./wayland/hyprland/hyprpaper.nix;
      hypridle = ./wayland/hyprland/hypridle.nix;
      hyprlock = ./wayland/hyprland/hyprlock.nix;
    };
    river = ./wayland/river.nix;
    swaync = ./wayland/swaync.nix;
    eww = ./wayland/eww.nix;
    screenshot = ./wayland/screenshot.nix;
    rofi = ./wayland/rofi.nix;
  };
}
