{ asMailServer ? false }:
let
  sources = import ../nix/sources.nix;
  mail = import ./email.nix { inherit pkgs; asServer = asMailServer; };
  thing = import sources.thing;
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
      htop
      httpie
      jq
      nodejs
      ranger
      thing
      ripgrep
      slack
      unzip
      wget
      yq
      zip
      sqlite
      graphviz
      nixfmt
    ];

    nixos = [
      discord
      firefox
      fdk_aac
      feh
      xournal
      chromium
      lua51Packages.luabitop
      lua
      steam
      mailcap
      w3m
      networkmanagerapplet
      killall
      xclip
      scrot
      gnome3.zenity
      muchsync
    ];

    programming = [ gnumake neovim vscode racket ];

    haskell = [ haskellPackages.hp2html haskellPackages.hp2pretty ghcid stack ];

    provers = [ agda agdaPackages.standard-library idris2 ];

    scala = [ jdk metals sbt ];

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

    browserpass = {
      enable = true;
      browsers = [ "firefox" ];
    };

    direnv = {
      enable = true;
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
# fish_color_param, the color for regular command parameters
# fish_color_comment, the color used for code comments
# fish_color_match, the color used to highlight matching parenthesis
# fish_color_selection, the color used when selecting text (in vi visual mode)
# fish_color_search_match, used to highlight history search matches and the selected pager item (must be a background)
# fish_color_operator, the color for parameter expansion operators like '*' and '~'
# fish_color_escape, the color used to highlight character escapes like '\n' and '\x70'
# fish_color_cwd, the color used for the current working directory in the default prompt
# fish_color_autosuggestion, the color used for autosuggestions
# fish_color_user, the color used to print the current username in some of fish default prompts
# fish_color_host, the color used to print the current host system in some of fish default prompts
# fish_color_host_remote, the color used to print the current host system in some of fish default prompts, if fish is running remotely (via ssh or similar)
# fish_color_cancel, the color for the '^C' indicator on a canceled command
# Additionally, the following variables are available to change the highlighting in the completion pager:
# 
# fish_pager_color_progress, the color of the progress bar at the bottom left corner
# fish_pager_color_background, the background color of a line
# fish_pager_color_prefix, the color of the prefix string, i.e. the string that is to be completed
# fish_pager_color_completion, the color of the completion itself
# fish_pager_color_description, the color of the completion description
# fish_pager_color_secondary_background, fish_pager_color_background of every second unselected completion. Defaults to fish_pager_color_background
# fish_pager_color_secondary_ prefix, fish_pager_color_prefix of every second unselected completion. Defaults to fish_pager_color_prefix
# fish_pager_color_secondary_completion, fish_pager_color_completion of every second unselected completion. Defaults to fish_pager_color_completion
# fish_pager_color_secondary_description, fish_pager_color_description of every second unselected completion. Defaults to fish_pager_color_description
# fish_pager_color_selected_background, fish_pager_color_background of the selected completion. Defaults to fish_color_search_match
# fish_pager_color_selected_prefix, fish_pager_color_prefix of the selected completion. Defaults to fish_pager_color_prefix
# fish_pager_color_selected_completion, fish_pager_color_completion of the selected completion. Defaults to fish_pager_color_completion
# fish_pager_color_selected_description, fish_pager_color_description of the selected completion. Defaults to fish_pager_color_description      '';
    };

    fzf = { enable = true; };

    git = {
      enable = true;
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
      ignores = [ "TAGS" ];
      userEmail = mail.gmail.userName;
      userName = mail.gmail.realName;
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
          use_symbol_for_status = true;
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
      enable = true;
      enableFishIntegration = true;
      options = [ "enhanced" "once" "fzf" ];
    };

    zathura = { enable = true; };

    astroid = { enable = true; };

    alot = { enable = true; };

    neomutt = {
      enable = true;
      extraConfig = ''
        auto_view text/html
        alternative_order text/enriched text/plain text/html text

        set index_format = "%Z %{%D} %-15.15L %-5.5Y %s"

        mailboxes "/home/vlad/Maildir/gmail/Sent"
        mailboxes "/home/vlad/Maildir/gmail/[Gmail]/Sent Mail"

        unbind *

        bind generic : enter-command
        bind generic q exit
        bind generic gg first-entry
        bind generic } half-down
        bind generic { half-up
        bind generic ? help
        bind generic G last-entry
        bind generic ^R refresh
        bind generic / search
        bind generic * search-next
        bind generic \# search-opposite
        bind generic x select-entry
        bind generic t tag-entry
        bind generic j next-entry
        bind generic k previous-entry

        bind index c copy-message
        bind index dd delete-message
        bind index <Space> display-message
        bind index f forward-message
        bind index m mail
        bind index j next-undeleted
        bind index | pipe-message
        bind index k previous-undeleted
        bind index q quit
        bind index r reply
        bind index l sidebar-next
        bind index h sidebar-prev
        bind index O sidebar-open
        bind index N toggle-new
        bind index v view-attachments
        bind index [ next-unread
        bind index ] previous-unread

        bind pager j next-line
        bind pager k previous-line
        bind pager c copy-message
        bind pager C decode-copy
        bind pager e edit
        bind pager L edit-label
        bind pager q exit
        bind pager f forward-message
        bind pager R group-reply
        bind pager m mail
        bind pager t modify-tags
        bind pager | pipe-message
        bind pager s save-message
        bind pager / search
        bind pager * search-next
        bind pager \# search-opposite
        bind pager l sidebar-next
        bind pager h sidebar-prev
        bind pager O sidebar-open
        bind pager gg top
        bind pager G bottom
        bind pager v view-attachments

        bind attach <Space> view-attach
        bind attach s save-entry

        bind compose x send-message
        bind compose a attach-file
        bind compose v view-attach
        bind compose c edit-cc
        bind compose b edit-bcc
        bind compose h edit-headers
        bind compose e edit-message
        bind compose s edit-subject
        bind compose t edit-to
        bind compose i ispell

        set nm_default_uri = /home/vlad/Maildir
      '';
      sidebar = {
        enable = true;
        shortPath = true;
      };
      sort = "reverse-threads";
    };

    mbsync.enable = asMailServer;
    msmtp.enable = true;
    notmuch = {
      enable = true;
      hooks.preNew = if asMailServer then "${pkgs.isync}/bin/mbsync --all" else "";
    };

  };

  #########################################################
  ## File
  file = {
    ".mailcap".text = ''
        text/html;  w3m -dump -o document_charset=%{charset} '%s'; nametemplate=%s.html; copiousoutput
      '';
    ".config/nvim/init.vim".source = ../config/nvim/init.vim;
    ".config/nvim/coc-settings.json".source = ../config/nvim/coc-settings.json;
    ".ghci".source = ../config/ghci;
    ".config/fish/functions/clip.fish".source = ../config/fish/functions/clip.fish;
    ".XCompose".source = ../config/XCompose;
  };

  #########################################################
  ## Services
  services = {
    mbsync.enable = asMailServer;
    stalonetray = {
      enable = true;
      package = pkgs.stalonetray;
      config = {
        decorations = "all";
        transparent = true;
        dockapp_mode = "none";
        geometry = "9x1-0+0";
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
  accounts.email.accounts = { gmail = mail.gmail; };
in {
  nixpkgs = nixpkgs;
  accounts = accounts;
  packages = packages;
  sessionVariables = sessionVariables;
  programs = programs // mkKitty { };
  services = services;
  helpers = { inherit mkKitty; };
  file = file;
}
