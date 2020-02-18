{ pkgs, ... }:

let
  pass = "${pkgs.pass}/bin/pass";
  head = "${pkgs.coreutils}/bin/head";
  getPassword = service: "${pass} show '${service}' | ${head} -n 1";
  feh4k = "${pkgs.feh}/bin/feh --bg-scale $HOME/.config/nixpkgs/wallpapers/4k/haskell_1.jpg";
  feh2k = "${pkgs.feh}/bin/feh --bg-scale $HOME/.config/nixpkgs/wallpapers/haskell.jpg";
  neovimNixpkgs = "https://github.com/nixos/nixpkgs/tarball/c8718e29b3740b9094aee842e7b157872d98942e";
in

{
  nixpkgs.config.allowUnfree = true;

  home.packages = with pkgs; [
    # user
    firefox
    haskellPackages.niv
    slack discord
    gnome3.nautilus

    # cli
    htop xclip scrot
    jq yq fpp tmux ripgrep
    wget httpie
    zip unzip

    # programming
    vscode
    gitAndTools.hub gnumake
    # hack because reasons
    (import (builtins.fetchTarball neovimNixpkgs) {}).pkgs.neovim

    # database
    dbmate sqlitebrowser

    # Haksell
    stack ghcid

    # other
    nodejs
    idris

    # K/java
    jre flex gcc z3
  ];

  programs.autorandr = {
    enable = true;
    profiles = {
      laptop = {
        fingerprint = {
          eDP1 = builtins.readFile ./programs/autorandr/eDP1.fingerprint;
        };
        config = {
          eDP1 = {
            enable = true;
            primary = true;
            position = "0x0";
            mode = "2560x1440";
            rate = "60.01";
          };
          DP1.enable = false;
          DP2.enable = false;
          HDMI1.enable = false;
          HDMI2.enable = false;
          VIRTUAL1.enable = false;
        };
        hooks.postswitch = feh2k;
      };

      home = {
        fingerprint = {
          eDP1 = builtins.readFile ./programs/autorandr/eDP1.fingerprint;
          DP2  = builtins.readFile ./programs/autorandr/asus-home.fingerprint;
        };
        config = {
          eDP1 = {
            enable = true;
            primary = false;
            position = "3840x0";
            mode = "2560x1440";
            rate = "60.01";
          };
          DP1.enable = false;
          DP2 = {
            enable = true;
            primary = true;
            mode = "3840x2160";
            rate = "60.00";
          };
          HDMI1.enable = false;
          HDMI2.enable = false;
          VIRTUAL1.enable = false;
        };
        hooks.postswitch = feh2k;
      };
    };
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
        disabled = false;
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

  services.compton.enable = true;

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
