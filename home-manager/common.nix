let
  sources = import ./nix/sources.nix;

  reddup = import sources.reddup;
  nixpkgs = import sources.nixpkgs { config.allowUnfree = true; };
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
      ripgrep
      slack
      unzip
      wget
      yq
      zip
    ];

    nixos = [
      discord
      firefox
      fdk_aac
      ffmpeg-full
      kitty
    ];

    programming = [
      gnumake
      neovim
      vscode
    ];

    haskell = [
      haskellPackages.hp2html
      haskellPackages.hp2pretty
      ghcid
      stack
    ];

    provers = [
    AgdaStdlib
    haskellPackages.Agda
    idris
    ];

    scala = [
      jdk
      metals
      sbt
    ];

    latex = [
      tectonic
      texlab
    ];

    streaming = [
      chatterino2
    ];

  };

  sessionVariables = {
    EDITOR = "nvim";
  };

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

    fzf = {
      enable = true;
    };

    git = {
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

    home-manager = {
      enable = true;
    };

    mpv = {
      enable = true;
    };

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

    zathura = {
        enable = true;
      };

  };

  #########################################################
  ## Services
  services = {
    picom.enable = true;
  };
in
{
  packages = packages;
  sessionVariables = sessionVariables;
  programs = programs;
  services = services;
}
