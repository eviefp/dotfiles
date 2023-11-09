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
          kitty.enable = true;

          bat = {
            enable = true;
            config = {
              theme = "TwoDark";
              pager = "less -FR";
            };
          };

          feh = {
            enable = true;
            keybindings = {
              zoom_in = [ "plus" "J" ];
              zoom_out = [ "minus" "K" ];
              zoom_default = "0";
              scroll_left = "h";
              scroll_right = "l";
              scroll_up = "k";
              scroll_down = "j";
              quit = "q";
              next_img = "Right";
              prev_img = "Left";
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
            nix-direnv = {
              enable = true;
            };
          };

          eza = {
            enable = true;
            enableAliases = true;
            git = true;
            icons = true;
            extraOptions = [
              "--group-directories-first"
              "--across"
              "--icons"
              "--classify"
            ];
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

              fish_vi_key_bindings

              bind -M insert \cr 'commandline -f history-pager'
            '';
            shellAliases = {
              # git
              gs = "${pkgs.git}/bin/git status";

              # bat
              cat = "${pkgs.bat}/bin/bat";

              # nix-shell
              nix-shell = "nix-shell --command ${pkgs.fish}/bin/fish";
              nix-pshell = "nix-shell --command ${pkgs.fish}/bin/fish --pure";
            };
          };

          fzf = {
            enable = true;
            enableBashIntegration = true;
            enableFishIntegration = true;
          };

          # https://github.com/shinzui/dotfiles.nix/issues/1
          gh = {
            enable = true;
            gitCredentialHelper.enable = true;
            settings = {
              git_protocol = "ssh";
              prompt = "enabled";
            };
          };

          git = {
            enable = true;
            delta = {
              enable = false;
              options = {
                navigate = true;
              };
            };
            difftastic = {
              enable = true;
              color = "auto";
              background = "dark";
              # display = "side-by-side";
            };
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

          ## Experimental
          hyfetch = {
            enable = true;
            settings = {
              backend = "neofetch";
              preset = "transgender";
              mode = "rgb";
              light_dark = "dark";
              lightness = 0.65;
              color_align.mode = "horizontal";
            };
          };

          joshuto = {
            enable = true;
          };

          # this is pretty janky on xmonad, also don't really need more UI
          eww = {
            enable = false;
            configDir = ./eww/.;
          };

        };
      })
      (lib.mkIf cfg.experimental {
        programs = {
          broot = {
            enable = true;
            enableFishIntegration = true;
            enableBashIntegration = true;
            settings = {
              modal = false;
              skin = { default = "none none / gray(20) none"; };
            };
          };

          lazygit = {
            enable = true;
          };
        };
      })
    ];
}
