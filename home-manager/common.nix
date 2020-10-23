let
  sources = import ./nix/sources.nix;
  reddup = import sources.reddup;
  thing = import sources.thing;
  nixpkgs = import sources.nixpkgs { config.allowUnfree = true; };
  pkgs = nixpkgs.pkgs;
  sowon = pkgs.stdenv.mkDerivation {
    buildInputs = [ pkgs.pkg-config pkgs.SDL2 ];
    name = "sowon";
    src = sources.sowon;
    installPhase = ''
      mkdir -p $out/bin
      cp digits.png $out/bin/
      cp sowon $out/bin/
    '';
  };
  # myEmacs = import ./emacs.nix { pkgs = pkgs; };

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
      sowon
      ripgrep
      slack
      unzip
      wget
      yq
      zip
      # myEmacs
    ];

    nixos = [
      discord
      firefox
      fdk_aac
      feh
    ];

    programming = [
      gnumake
      neovim
      vscode
      racket
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
      ffmpeg-full
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

  ## Helpers
  mkKitty = extraSettings: {
    kitty = {
      enable = true;
      settings = {
        "font_family"        = "Hasklug Nerd Font Complete Mono";
        "bold_font"          = "Hasklug Bold Nerd Font Complete Mono";
        "italic_font"        = "Hasklug Italic Nerd Font Complete Mono";
        "bold_italic_font"   = "Hasklug Bold Italic Nerd Font Complete Mono";
        "disable_ligatures"  = "never";
        "font_size"          = "16.0";
        "background_opacity" = "0.8";
        "background"         = "#2b2b2b";
      } // extraSettings;
    };
  };
in
{
  packages = packages;
  sessionVariables = sessionVariables;
  programs = programs // mkKitty {};
  services = services;
  helpers = {
    inherit mkKitty;
  };
}
