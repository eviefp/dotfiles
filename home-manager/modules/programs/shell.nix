/****************************************************************************
  * programs/shell module
  *
  * Enable packages I use for shell-related things:
  *   - 'killall' to easily kill processes
  *   - 'ripgrep' for navigation
  *   - 'wget'
  *   - 'unzip'
  *   - 'zip'
  **************************************************************************/
{ lib, config, pkgs, ... }:
let cfg = config.evie.programs.shell;
in
{
  imports = [ ];

  options.evie.programs.shell = {
    enable = lib.options.mkEnableOption "Enable shell tools.";
    experimental = lib.options.mkEnableOption "Enable experimental shell tools.";
  };

  config = lib.mkMerge
    [
      (lib.mkIf cfg.enable {
        home.packages = [
          pkgs.killall
          pkgs.ripgrep
          pkgs.wget
          pkgs.unzip
          pkgs.zip
        ];

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

          direnv = {
            enable = true;
            enableBashIntegration = true;
            enableFishIntegration = true;
          };

          exa = {
            enable = true;
            enableAliases = true;
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

              # bat
              cat = "${pkgs.bat}/bin/bat";
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

          home-manager = {
            enable = true;
          };

          jq = {
            enable = true;
          };

          nix-index = {
            enable = true;
            enableFishIntegration = true;
            enableBashIntegration = true;
          };

          ncspot = {
            enable = true;
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

          zoxide = {
            enable = true;
            enableBashIntegration = true;
            enableFishIntegration = true;
          };

        };
      })
      (lib.mkIf cfg.experimental {
        programs = {
          broot = {
            enable = true;
            enableFishIntegration = true;
            enableBashIntegration = true;
            modal = true;
            skin = { default = "none none / gray(20) none"; };
          };

          lazygit = {
            enable = true;
          };
        };
      })
    ];
}