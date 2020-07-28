let
  sources = import ./nix/sources.nix;

  reddup = import sources.reddup;
  nixpkgs = import sources.nixpkgs { config.allowUnfree = true; };
  pkgs = nixpkgs.pkgs;

  pass = "${pkgs.pass}/bin/pass";
  head = "${pkgs.coreutils}/bin/head";
  getPassword = service: "${pass} show '${service}' | ${head} -n 1";

in

{
  nixpkgs.config.nixpkgs.config.allowUnfree = true;

  home.packages = with pkgs; [
    # user
    haskellPackages.niv
    firefox
    discord
    slack
    gnome3.nautilus
    calibre
    jitsi
    ffmpeg-full fdk_aac

    # cli
    htop xclip scrot
    jq yq fpp tmux ripgrep
    wget httpie
    zip unzip

    # programming
    vscode
    gitAndTools.hub gnumake
    neovim

    # database
    dbmate sqlitebrowser

    # Haksell
    stack ghcid haskellPackages.hp2html haskellPackages.hp2pretty

    # other
    nodejs
    idris
    haskellPackages.Agda AgdaStdlib

    # scala
    jdk sbt metals

    # K/java
    flex gcc z3

    texlab
    tectonic

    # testing
    chatterino2 xmagnify
    kitty
    reddup
  ];

  home.sessionVariables = {
    EDITOR = "nvim";
  };

  programs.bat = {
    enable = true;
    config = {
      theme = "TwoDark";
      pager = "less -FR";
    };
  };

  programs.broot = {
    enable = true;
    enableFishIntegration = true;
  };

  programs.browserpass = {
    enable = true;
    browsers = [ "firefox" ];
  };

  programs.direnv = {
    enable = true;
    enableFishIntegration = true;
  };

  programs.fish = {
    enable = true;
    package = pkgs.fish;
  };

  programs.fzf = {
    enable = true;
  };

  programs.mpv = {
    enable = true;
  };

  programs.obs-studio = {
    enable = true;
    # broken
    # plugins = [ pkgs.obs-linuxbrowser ];
  };

  programs.starship = {
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

  programs.z-lua = {
    enable = true;
    enableFishIntegration = true;
    options = [ "enhanced" "once" "fzf" ];
  };

  programs.zathura = {
    enable = true;
  };

  services.picom.enable = true;

  programs.git = {
    enable = true;
    aliases = {
      lol = "log --graph --decorate --oneline --abbrev-commit";
      lola = "log --graph --decorate --oneline --abbrev-commit --all";
      hist = "log --pretty=format:'%h %ad | %s%d [%an]' --graph --date=short";
      lg = "log --color --graph --pretty=format:'%Cred%h$Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --";
      recent = "for-each-ref --sort=committerdate refs/heads/ --format='%(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(color:red)%(objectname:short)%(color:reset) - %(contents:subject) - %(authorname) (%(color:green)%(committerdate:relative)%(color:reset))'";
      work = "log --pretty=format:'%h%x09%an%x09%ad%x09%s'";
    };
    ignores = [ "TAGS" ];
    userEmail = "admin@cvlad.info";
    userName = "Vladimir Ciobanu";
  };

  programs.home-manager = {
    enable = true;
  };

}
