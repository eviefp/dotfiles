{
  chat = ./chat.nix;
  gui = ./gui.nix;
  browser = {
    firefox = ./browser/firefox.nix;
    qutebrowser = ./browser/qutebrowser.nix;
  };
  streaming = ./streaming.nix;
  twitch-tui = ./twitch-tui.nix;
}
