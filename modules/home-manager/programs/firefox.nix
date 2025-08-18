/****************************************************************************
  * programs/firefox module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.programs.firefox;
  quickmarks = {
    tw = "https://twitch.tv/directory/all";
    gm = "https://www.group-meowing.ro/";
    hm = "https://nix-community.github.io/home-manager/options.xhtml";
  };
  go = str: key: url: ''
    ${str}
    bind go${key} open ${url}
    bind gt${key} tabopen ${url}
    bind gw${key} winopen ${url}

  '';
  generatedQuickmarks = lib.attrsets.foldlAttrs go "" quickmarks;
in
{
  options.evie.programs.firefox = {
    enable = lib.mkEnableOption "firefox defaults";
  };

  config = lib.mkIf cfg.enable {
    # TODO:
    # - native
    # - editor
    home.file = {
      ".config/tridactyl/tridactylrc".text = ''
        colourscheme dark

        set editorcmd kitty -e nvim "%f" "+normal!%lGzv%c|"
        set allowautofocus false
        set searchengine duckduckgo

        set hintfiltermode vimperator-reflow
        set hintchars fdsqjklmrezauiopwxcvghtybn

        unbind gt
        unbind gT
        unbind go
        unbind gO

        bind --mode=normal J tabnext
        bind --mode=normal K tabprev

        bind --mode=normal o fillcmdline open
        bind --mode=normal go current_url open
        bind --mode=normal O fillcmdline tabopen
        bind --mode=normal gO current_url tabopen

        bind / fillcmdline find
        bind ? fillcmdline find --reverse
        bind n findnext --search-from-view
        bind N findnext --search-from-view --reverse
        bind gn findselect
        bind gN composite findnext --search-from-view --reverse; findselect
        bind ,<Space> nohlsearch

        bind --mode=browser <C-.> sidebartoggle

        bind q mute toggle

        bind <C-p> pin
        bind <C-P> unpin

        ${generatedQuickmarks}

        " Lose most features but keep keybindings for buggy sites: https://github.com/tridactyl/tridactyl/issues/5050
        seturl ^https://nix-community.github.io/home-manager/options.xhtml noiframe true

      '';
      ".mozilla/native-messaging-hosts/passff.json".source =
        "${pkgs.passff-host}/share/passff-host/passff.json";
      ".mozilla/native-messaging-hosts/firenvim.json".text = ''
        { "name": "firenvim", "description": "Turn your browser into a Neovim GUI.", "path": "/home/evie/.local/share/firenvim/firenvim", "type": "stdio", "allowed_extensions": ["firenvim@lacamb.re"]}
      '';
    };

    programs = {
      firefox = {
        enable = true;
        package = pkgs.firefox.override {
          nativeMessagingHosts = [ pkgs.tridactyl-native ];
          cfg = {
            pipewireSupport = true;
            ffmpegSupport = true;
            smartcardSupport = true;
          };
        };
        policies = {
          AllowFileSelctionDialogs = true;
          AppAutoUpdate = false;
          DefaultDownloadDirectory = "~/Downloads";
          DisableAppUpdate = true;
          DisableFirefoxStudies = true;
          DisablePocket = true;
          DisableSetDesktopBackground = true;
          DisableTelemetry = true;
          DisableBookmarksToolbar = true;
          DisplayMenuBar = false;
          DontCheckDefaultBrowser = true;
          SearchBar = "unified";
          ShowHomeButton = false;
        };
      };
    };
  };
}
