/****************************************************************************
  * Programs module
  *
  * (Console) programs I use. They are visually split by category here, which
  * indicates how I would split them by (sub)modules later, if needed.
  **************************************************************************/
{ lib, config, pkgs, ... }:
let cfg = config.evie.programs;
in
{
  imports = [
    ./programs/dev/nix.nix
    ./programs/dev/tools.nix
    ./programs/text.nix
    ./programs/shell.nix
    ./programs/shell/ranger.nix
  ];

  options.evie.programs = {
    enable = lib.options.mkEnableOption "Enable generic packages.";
    haskell = lib.options.mkEnableOption "Enable Haskell packages.";
    provers = lib.options.mkEnableOption "Enable provers packages.";
    streaming = lib.options.mkEnableOption "Enable streaming packages.";
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      nixpkgs.config.allowUnfree = true;

      home.packages = [
        # Programming related

        pkgs.graphviz
        pkgs.nodejs

        # Fonts
        pkgs.fira-code
        pkgs.nerdfonts

        # Mail

        # Chat
        pkgs.discord
        pkgs.slack

        pkgs.pipewire # used to create multi audio sinks

        # Might not need
        pkgs.gnome3.zenity
        pkgs.lua
        pkgs.lua51Packages.luabitop
        pkgs.networkmanagerapplet
      ];

      fonts.fontconfig.enable = true;

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
          skin = { default = "none none / gray(20) none"; };
        };

        exa = {
          enable = true;
          enableAliases = true;
        };

        jq = { enable = true; };

        lazygit = { enable = true; };

        nix-index = {
          enable = true;
          enableFishIntegration = true;
          enableBashIntegration = true;
        };

        ncspot = { enable = true; };

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
            hist =
              "log --pretty=format:'%h %ad | %s%d [%an]' --graph --date=short";
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

        zoxide = {
          enable = true;
          enableBashIntegration = true;
          enableFishIntegration = true;
        };

        zathura = { enable = true; };
      };
    })
    (lib.mkIf cfg.haskell {
      home.packages = [
        pkgs.haskellPackages.hp2html
        pkgs.haskellPackages.hp2pretty
        pkgs.stack
      ];
      home.file = { ".ghci".source = ../../config/ghci; };
    })
    (lib.mkIf cfg.provers {
      home.packages =
        [ pkgs.agda pkgs.agdaPackages.standard-library pkgs.idris2 ];
    })
    (lib.mkIf cfg.streaming {
      home.packages = [ pkgs.ffmpeg-full pkgs.chatterino2 ];

      programs = {
        obs-studio = {
          enable = true;
          plugins = [ ];
        };
      };
    })
  ];
}
