{
  common = ./common.nix;
  system = ./system.nix;
  fonts = ./fonts.nix;
  sops = ./sops.nix;

  email = ./email.nix;
  bower = ./programs/bower.nix;
  ect = ./programs/ect.nix;

  calendar = ./calendar.nix;

  programs = {
    term = ./programs/term.nix;
    kitty = ./programs/kitty.nix;
    ranger = ./programs/shell/ranger.nix;

    browsers = ./programs/browsers.nix;
    chat = ./programs/chat.nix;
    streaming = ./programs/streaming.nix;

    dev = {
      default = ./programs/dev.nix;
      haskell = ./programs/dev/haskell.nix;
      lua = ./programs/dev/lua.nix;
      nix = ./programs/dev/nix.nix;
      provers = ./programs/dev/provers.nix;
      tools = ./programs/dev/tools.nix;
    };

    text = ./programs/text.nix;
  };

  editors = {
    emacs = ./programs/editors/emacs.nix;
    neovim = ./programs/editors/neovim.nix;
    helix = ./programs/editors/helix.nix;
  };

  gui = ./gui.nix;

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
