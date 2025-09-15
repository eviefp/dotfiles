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

  # autocontain = container: url: acc: "${acc}autocontain -s ${lib.strings.replaceString "." "\\." url} ${container}\n";
  # generateAutocontain = container: xs: lib.lists.foldr (autocontain container) "" xs;
in
{
  options.evie.programs.firefox = {
    enable = lib.mkEnableOption "firefox defaults";
  };

  config = lib.mkIf cfg.enable {
    home.file = {
      # TODO: run :sanitise on first install
      ".config/tridactyl/tridactylrc".text = ''
        colourscheme dark

        set editorcmd kitty -e nvim "%f" "+normal!%lGzv%c|"
        set allowautofocus false
        set searchengine duckduckgo
        set autocontainmode relaxed

        set hintfiltermode vimperator-reflow
        set hintchars fdsqjklmrezauiopwxcvghtybn

        unbind gt
        unbind gT
        unbind go
        unbind gO
        unbind GO
        unbind GT
        unbind d
        unbind D
        unbind T
        unbind O

        bind --mode=normal J tabnext
        bind --mode=normal K tabprev

        bind --mode=normal o fillcmdline open
        bind --mode=normal gO current_url open
        bind --mode=normal t fillcmdline tabopen
        bind --mode=normal gT current_url tabopen

        bind --mode=normal O fillcmdline open -c work
        bind --mode=normal T fillcmdline tabopen -c work

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
          "3rdparty" = {
            Extensions = {
              "uBlock0@raymondhill.net" = {
                permissions = [ "internal:privateBrowsingAllowed" ];
                origins = [ ];
              };
              "tridactyl.vim@cmcaine.co.uk" = {
                permissions = [ "internal:privateBrowsingAllowed" ];
                origins = [ ];
              };
            };
          };

          AllowFileSelctionDialogs = true;
          AppAutoUpdate = false;
          AutofillAddressEnabled = false;
          AutofillCreditCardEnabled = false;

          BlockAboutAddons = false;
          BlockAboutConfig = false;
          BlockAboutProfiles = false;
          BlockAboutSupport = false;

          Cookies = {
            Behavior = "reject-tracker";
          };

          DefaultDownloadDirectory = "~/Downloads";
          DisableAppUpdate = true;
          DisableBookmarksToolbar = true;
          DisableFirefoxStudies = true;
          DisablePocket = true;
          DisableSetDesktopBackground = true;
          DisableTelemetry = true;
          DisplayMenuBar = false;
          DontCheckDefaultBrowser = true;

          EnableTrackingProtection = {
            Value = true;
            Cryptomining = true;
            Fingerprinting = true;
            EmailTracking = true;
            SuspectedFingerprinting = true;
          };

          FirefoxHome = {
            Pocket = false;
            Snippets = false;
          };
          FirefoxSuggest = {
            WebSuggestions = false;
            SponsoredSuggestions = false;
            ImproveSuggest = false;
          };

          HardwareAcceleration = true;

          PasswordManagerEnabled = false;

          # Permissions = { };
          # PopupBlocking

          SearchBar = "unified";
          SearchSuggestEnabled = false;
          SecurityDevices = {
            "OpenSC PKCS#11 Module" = "${pkgs.opensc}/lib/opensc-pkcs11.so";
          };
          ShowHomeButton = false;
          SkipTermsOfUse = true;
          SupportMenu = false;

          TranslateEnabled = true;
        };

        profiles = {
          evie = {
            isDefault = true;
            containers = {
              # icon can be fingerprint, briefcase, dollar, cart, vacation, gift, food, fruit, pet, tree, chill, circle, fence
              # color can be blue, turquoise, green, yellow, orange, red, pink, purple, toolbar
              work = {
                color = "yellow";
                icon = "briefcase";
                id = 1;
              };
              personal = {
                color = "pink";
                icon = "tree";
                id = 2;
              };
              facebook = {
                color = "blue";
                icon = "fingerprint";
                id = 3;
              };
            };
            containersForce = true;

            extensions = {
              packages = with pkgs.nur.repos.rycee.firefox-addons; [
                ublock-origin-upstream
                betterttv
                clearurls
                darkreader
                privacy-badger
                stylus
                tridactyl
                facebook-container
                passff
              ];
              settings = {
                # "tridactyl.vim@cmcaine.co.uk" = { };
              };
              force = true;
            };

            search = {
              default = "ddg";
              order = [ "ddg" ];
              force = true;
            };

            settings = {
              "accessibility.typeaheadfind.flashBar" = 0;
              "accessibility.force_disabled" = 1;

              "app.normandy.api_url" = "";
              "app.normandy.enabled" = false;

              "browser.aboutConfig.showWarning" = false;
              "browser.engagement.ctrlTab.has-used" = true;
              "browser.engagement.downloads-button.has-used" = true;
              "browser.engagement.fxa-toolbar-menu-button.has-used" = true;
              "browser.engagement.home-button.has-removed" = true;
              "browser.engagement.home-button.has-used" = true;
              "browser.engagement.library-button.has-used" = true;
              "browser.engagement.sidebar-button.has-used" = true;
              "browser.newtab.extensionControlled" = true;
              "browser.newtab.privateAllowed" = true;
              "browser.newtabpage.activity-stream.feeds.topsites" = false;
              "browser.newtabpage.activity-stream.showSearch" = false;
              "browser.newtabpage.enabled" = false;
              "browser.search.region" = "RO";
              "browser.search.serpEventTelemetryCategorization.regionEnabled" = false;
              "browser.startup.homepage" = "about:preferences";
              "browser.startup.page" = 3;
              "browser.tabs.inTitlebar" = 1;
              "browser.theme.content-theme" = 0;
              "browser.theme.toolbar-theme" = 0;
              "browser.toolbarbuttons.introduced.sidebar-button" = true;
              "browser.toolbars.bookmarks.visibility" = "never";
              "browser.topsites.contile.cachedTiles" = "[]";

              "browser.uiCustomization.horizontalTabstrip" = "[\"firefox-view-button\",\"tabbrowser-tabs\",\"new-tab-button\"]";
              # this is annoying and hard to read, but customizes the top bar the way i want it.
              "browser.uiCustomization.state" = "{\"placements\":{\"widget-overflow-fixed-list\":[\"panic-button\",\"history-panelmenu\",\"zoom-controls\"],\"unified-extensions-area\":[\"jid1-mnnxcxisbpnsxq_jetpack-browser-action\",\"addon_darkreader_org-browser-action\",\"_74145f27-f039-47ce-a470-a662b129930a_-browser-action\",\"_7a7a4a92-a2a0-41d1-9fd7-1e92480d612d_-browser-action\"],\"nav-bar\":[\"back-button\",\"forward-button\",\"alltabs-button\",\"firefox-view-button\",\"downloads-button\",\"ublock0_raymondhill_net-browser-action\",\"urlbar-container\",\"vertical-spacer\",\"unified-extensions-button\",\"preferences-button\"],\"toolbar-menubar\":[\"menubar-items\"],\"TabsToolbar\":[],\"vertical-tabs\":[\"tabbrowser-tabs\"],\"PersonalToolbar\":[\"import-button\",\"personal-bookmarks\"]},\"seen\":[\"addon_darkreader_org-browser-action\",\"jid1-mnnxcxisbpnsxq_jetpack-browser-action\",\"_74145f27-f039-47ce-a470-a662b129930a_-browser-action\",\"ublock0_raymondhill_net-browser-action\",\"_7a7a4a92-a2a0-41d1-9fd7-1e92480d612d_-browser-action\",\"developer-button\",\"screenshot-button\"],\"dirtyAreaCache\":[\"unified-extensions-area\",\"nav-bar\",\"TabsToolbar\",\"vertical-tabs\",\"PersonalToolbar\",\"toolbar-menubar\",\"widget-overflow-fixed-list\"],\"currentVersion\":23,\"newElementCount\":2}";

              "browser.urlbar.placeholderName" = "DuckDuckGo";
              "browser.urlbar.placeholderName.private" = "DuckDuckGo";
              "browser.urlbar.quicksuggest.migrationVersion" = 2;
              "browser.urlbar.quicksuggest.scenario" = "history";
              "browser.urlbar.suggest.bookmark" = false;
              "browser.urlbar.suggest.engines" = false;
              "browser.urlbar.suggest.history" = false;
              "browser.urlbar.suggest.openpage" = false;
              "browser.urlbar.suggest.topsites" = false;

              "browser.tabs.warnOnClose" = true;
              "browser.warnOnQuitShortcut" = false;

              "devtools.everOpened" = true;
              "devtools.inspector.activeSidebar" = "computedview";
              "devtools.inspector.selectedSidebar" = "computedview";

              "extensions.activeThemeID" = "firefox-compact-dark@mozilla.org";
              "extensions.autoDisableScopes" = 0;
              "extensions.blocklist.pingCountVersion" = -1;
              "extensions.colorway-builtin-themes-cleanup" = 1;
              "extensions.databaseSchema" = 37;
              "extensions.incognito.migrated" = true;
              "extensions.quarantineIgnoredByUser.tridactyl.vim@cmcaine.co.uk" = true;
              "extensions.quarantinedDomains.list" = "";
              "extensions.signatureCheckpoint" = 1;
              "extensions.systemAddonSet" = "{\"schema\":1,\"addons\":{}}";
              "extensions.ui.dictionary.hidden" = true;
              "extensions.ui.extension.hidden" = true;
              "extensions.ui.locale.hidden" = true;
              "extensions.ui.mlmodel.hidden" = true;
              "extensions.ui.sitepermission.hidden" = true;
              "extensions.unifiedExtensions.button.always_visible" = false;
              "extensions.webcompat.enable_picture_in_picture_overrides" = true;
              "extensions.webcompat.enable_shims" = true;
              "extensions.webcompat.perform_injections" = true;
              "extensions.webcompat.perform_ua_overrides" = true;

              "font.internaluseonly.changed" = false;

              "gecko.handlerService.defaultHandlersVersion" = 1;

              "layout.css.prefers-color-scheme.content-override" = 0;

              "sidebar.backupState" = "{\"command\":\"\",\"panelOpen\":false,\"launcherWidth\":55,\"launcherExpanded\":false,\"launcherVisible\":true}";
              "sidebar.main.tools" = "syncedtabs,history,bookmarks";
              "sidebar.new-sidebar.has-used" = true;
              "sidebar.old-sidebar.has-used" = true;
              "sidebar.position_start" = false;
              "sidebar.revamp" = true;
              "sidebar.verticalTabs" = true;

              "privacy.userContext.enabled" = true;
              "privacy.userContext.extension" = "tridactyl.vim@cmcaine.co.uk";
              "privacy.userContext.ui.enabled" = true;

              "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
            };

            userChrome = ''
              #statuspanel {
                bottom: 0% !important;
                right: 0% !important;
                overflow: hidden;
              }

              #statuspanel-label {
                height: 22px;
                text-align: right !important;
                margin-left: 50% !important;
                margin-right: 20px !important;
              }
            '';
          };
        };
      };
    };
  };
}
