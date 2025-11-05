{
  chat = ./chat.nix;
  gui = ./gui.nix;
  browser = {
    firefox = ./browser/firefox.nix;
    qutebrowser = ./browser/qutebrowser.nix;
    nyxt = ./browser/nyxt.nix;
  };
  streaming = ./streaming.nix;
  twitch-tui = ./twitch-tui.nix;
}
