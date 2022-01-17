let
  sources = import ../nix/sources.nix;
  emacsOverlay = import sources.emacs-overlay;
  nixpkgs = import sources.nixpkgs {
    config.allowUnfree = true;
    overlays = [ emacsOverlay ];
  };

  pkgs = nixpkgs.pkgs;

  #########################################################
  ## Packages
  packages = with pkgs; {
    generic = [
      haskellPackages.niv
      httpie
      nodejs
      ranger
      ripgrep
      slack discord
      unzip
      wget
      zip
      sqlite
      graphviz
      nixfmt
      ispell
    ];

    nixos = [
      nix-index
      firefox
      fdk_aac
      feh
      xournal
      chromium
      lua51Packages.luabitop
      lua
      pipewire
      mailcap
      w3m
      networkmanagerapplet
      killall
      xclip
      scrot
      gnome3.zenity
      nerdfonts fira-code
      steam
      pandoc
      haskellPackages.xmobar
      dmenu
      pass passff-host
      pinentry_gnome transmission-gtk pavucontrol
      paprefs xdg_utils
    ];

    programming = [ git gnumake neovim vscode ];

    haskell = [ haskellPackages.hp2html haskellPackages.hp2pretty ghcid stack ];

    provers = [ agda agdaPackages.standard-library idris2 ];

    latex = [ tectonic texlab ];

    streaming = [ ffmpeg-full chatterino2 ];
  };

  sessionVariables = { EDITOR = "emacs"; };

  #########################################################
  ## Programs
  programs = {

    bat = {
      enable = true;
      config = {
        theme = "TwoDark";
        pager = "less -FR";
      };
    };

    bottom = {
      enable = true;
      settings = {
        left_legend = true;
        temperature_type = "c";
        color = "gruvbox";
      };
    };

    broot = {
      enable = true;
      enableFishIntegration = true;
      enableBashIntegration = true;
      modal = true;
    };

    exa = {
      enable = true;
      enableAliases = true;
    };

    jq = {
      enable = true;
    };

    lazygit = {
      enable = true;
    };

    nix-index = {
      enable = true;
      enableFishIntegration = true;
      enableBashIntegration = true;
    };

    browserpass = {
      enable = true;
      browsers = [ "firefox" ];
    };

    direnv = {
      enable = true;
      enableBashIntegration = true;
      enableFishIntegration = true;
    };

    fish = {
      enable = true;
      package = pkgs.fish;
      interactiveShellInit = ''
set fish_color_normal "#a4c337"
set fish_color_command "#77c337"
set fish_color_quote "#37c393"
set fish_color_redirection "#37b5c3"
set fish_color_end "#3776c3"
set fish_color_error "#c33759"
'';
      shellAliases = {
        # exa
        ls = "${pkgs.exa}/bin/exa";
        ll = "${pkgs.exa}/bin/exa -l";
        la = "${pkgs.exa}/bin/exa -a";
        lt = "${pkgs.exa}/bin/exa --tree";
        lla = "${pkgs.exa}/bin/exa -la";

        # git
        gs = "${pkgs.git}/bin/git status";
      };
    };

    fzf = {
      enable = true;
      enableBashIntegration = true;
      enableFishIntegration = true;
    };

    git = {
      enable = true;
      delta.enable = true;
      aliases = {
        lol = "log --graph --decorate --oneline --abbrev-commit";
        lola = "log --graph --decorate --oneline --abbrev-commit --all";
        hist = "log --pretty=format:'%h %ad | %s%d [%an]' --graph --date=short";
        lg =
          "log --color --graph --pretty=format:'%Cred%h$Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --";
        recent =
          "for-each-ref --sort=committerdate refs/heads/ --format='%(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(color:red)%(objectname:short)%(color:reset) - %(contents:subject) - %(authorname) (%(color:green)%(committerdate:relative)%(color:reset))'";
        work = "log --pretty=format:'%h%x09%an%x09%ad%x09%s'";
      };
      extraConfig = {
        init.defaultBranch = "main";
        pull.ff = "only";
        merge.conflictstyle = "diff3";
      };
      ignores = [ "TAGS" ];
      userEmail = "me@eevie.ro";
      userName = "Evie Ciobanu";
    };

    home-manager = { enable = true; };

    mpv = { enable = true; };

    obs-studio = {
      enable = true;
      plugins = [ ];
    };

    starship = {
      enable = true;
      enableFishIntegration = true;
      settings = {
        add_newline = false;

        character = {
          success_symbol = "[λ](bold green)";
          error_symbol = "[✗](bold red)";
          vicmd_symbol = "[λ](bold yellow)";
        };

        cmd_duration = {
          min_time = 100;
          format = "underwent [$duration]($style) ";
        };

        nix_shell = {
          disabled = false;
          impure_msg = "[impure](bold red)";
          pure_msg = "[pure](bold green)";
          format = "[$state](bold blue) ";
        };
      };
    };

    z-lua = {
      enable = false;
      enableFishIntegration = true;
      options = [ "enhanced" "once" "fzf" ];
    };

    zoxide = {
      enable = true;
      enableBashIntegration = true;
      enableFishIntegration = true;
    };

    zathura = { enable = true; };

  };

  #########################################################
  ## File
  file = {
    ".config/nvim/init.vim".source = ../config/nvim/init.vim;
    ".config/nvim/coc-settings.json".source = ../config/nvim/coc-settings.json;
    ".config/tridactyl/tridactylrc".source = ../config/tridactyl;
    ".ghci".source = ../config/ghci;
    ".config/fish/functions/clip.fish".source = ../config/fish/functions/clip.fish;
    ".XCompose".source = ../config/XCompose;
    ".mozilla/native-messaging-hosts/passff.json".source = "${pkgs.passff-host}/share/passff-host/passff.json";
    ".mozilla/native-messaging-hosts/tridactyl.json".source = "${pkgs.tridactyl-native}/lib/mozilla/native-messaging-hosts/tridactyl.json";
  };

  #########################################################
  ## Services
  services = {
    stalonetray = {
      enable = false;
      package = pkgs.stalonetray;
      config = {
        decorations = "all";
        transparent = false;
        dockapp_mode = "none";
        geometry = "6x1-0+0";
        background = "#000000";
        kludges = "force_icons_size";
        grow_gravity = "NW";
        icon_gravity = "NE";
        icon_size = 20;
        sticky = true;
        window_strut = "auto";
        window_type = "dock";
        window_layer = "bottom";
        no_shrink = false;
        skip_taskbar = true;
      };
    };
  };

  fonts.fontconfig.enable = true;

  ## Helpers
  mkKitty = extraSettings: {
    kitty = {
      enable = true;
      settings = {
        "font_family" = "Hasklug Nerd Font Complete Mono";
        "bold_font" = "Hasklug Bold Nerd Font Complete Mono";
        "italic_font" = "Hasklug Italic Nerd Font Complete Mono";
        "bold_italic_font" = "Hasklug Bold Italic Nerd Font Complete Mono";
        "disable_ligatures" = "never";
        "font_size" = "16.0";
        "background_opacity" = "0.9";
        "background" = "#2b2b2b";
      } // extraSettings;
    };
  };
in {
  nixpkgs = nixpkgs;
  fonts = fonts;
  packages = packages;
  sessionVariables = sessionVariables;
  programs = programs // mkKitty { };
  services = services;
  helpers = { inherit mkKitty; };
  file = file;
}
