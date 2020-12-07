let
  sources = import ./nix/sources.nix;
  mail = import ./email.nix { inherit pkgs; };
  reddup = import sources.reddup;
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
      reddup
      thing
      ripgrep
      slack
      unzip
      wget
      yq
      zip
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
    ];

    programming = [ gnumake neovim vscode racket ];

    haskell = [ haskellPackages.hp2html haskellPackages.hp2pretty ghcid stack ];

    provers = [ AgdaStdlib haskellPackages.Agda idris ];

    scala = [ jdk metals sbt ];

    latex = [ tectonic texlab ];

    streaming = [ ffmpeg-full chatterino2 ];

  };

  sessionVariables = { EDITOR = "nvim"; };

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
      # broken ?
      plugins = [ pkgs.obs-linuxbrowser ];
    };

    starship = {
      enable = true;
      enableFishIntegration = true;
      settings = {
        add_newline = true;

        character = {
          symbol = "λ";
          error_symbol = "✗";
          use_symbol_for_status = true;
        };

        cmd_duration = {
          min_time = 100;
          prefix = "underwent ";
        };

        haskell = {
          symbol = " ";
          disabled = true;
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

    mbsync.enable = true;
    msmtp.enable = true;
    notmuch = {
      enable = true;
      hooks.preNew = "mbsync --all";
    };

  };

  #########################################################
  ## File
  file = {
    ".mailcap" = {
      text = ''
        text/html;  w3m -dump -o document_charset=%{charset} '%s'; nametemplate=%s.html; copiousoutput
      '';
    };
  };

  #########################################################
  ## Services
  services = {
    picom.enable = true;
    mbsync.enable = true;
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
        "background_opacity" = "0.8";
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
