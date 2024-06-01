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
{ pkgs, ... }:
let
  exp = pkgs.writeShellScriptBin "exp" ''
    echo 'cat foo | choose 2 1:-1'
    echo 'dust -- du replacement'
    echo 'sd find replace file'
    echo 'joshuto - ranger replacement'
    echo 'atuin - cmd find?'
    echo 'fd - find'
  '';
in
{
  imports = [ ];

  config = {
    home.packages = [
      pkgs.killall
      pkgs.ripgrep
      pkgs.wget
      pkgs.unzip
      pkgs.zip

      # experiments
      pkgs.choose
      pkgs.du-dust
      pkgs.sd

      pkgs.sops

      exp
    ];

    programs = {
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
        enableNushellIntegration = true;
        nix-direnv = {
          enable = true;
        };
      };

      eza = {
        enable = true;
        git = true;
        icons = true;
        extraOptions = [
          "--group-directories-first"
          "--across"
          "--icons"
          "--classify"
        ];
        enableBashIntegration = true;
        enableFishIntegration = true;
        enableNushellIntegration = true;
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
        signing = {
          key = "me@eevie.ro";
          signByDefault = true;
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
        enable = false;
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
        settings = {
          command_ley = ":";
          initial_screen = "library";
          use_nerdfont = true;
          notify = true;
          keybindings = {
            "?" = "help";
            "q" = "quit";

            "Space" = "playpause";
            "r" = "repeat";
            "s" = "shuffle";

            "Shift+h" = "prev";
            "Shift+l" = "next";

            "Ctrl+s" = "focus search";
            "Ctrl+q" = "focus queue";
            "Ctrl+l" = "focus library";
            "Ctrl+c" = "share current";
          };
        };
      };

      starship = {
        enable = true;
        enableFishIntegration = true;
        enableNushellIntegration = true;
        settings = {
          add_newline = true;

          character = {
            success_symbol = "[λ](bold green)";
            error_symbol = "[✗](bold red)";
            vicmd_symbol = "[λ](bold yellow)";
          };

          cmd_duration = {
            min_time = 100;
            format = "underwent [$duration]($style) ";
            show_notifications = false;
          };

          git_branch = {
            format = "'[$symbol$branch(:$remote_branch)]($style) ";
          };

          nix_shell = {
            disabled = false;
            format = "[$symbol$state]($style) ";
            symbol = "❄️";
          };

          hostname = {
            ssh_only = false;
            format = "[$ssh_symbol$hostname]($style) ";
          };

          shell = {
            disabled = true;
            fish_indicator = "";
            bash_indicator = "󱆃";
            nu_indicator = "";
          };

          username = {
            style_user = "bold pink";
            show_always = true;
            format = "[$user]($style)@";
          };

          gcloud = {
            disabled = true;
          };
        };
      };

      zoxide = {
        enable = true;
        enableBashIntegration = true;
        enableFishIntegration = true;
        enableNushellIntegration = true;
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

      nushell = {
        enable = true;
        configFile.text = ''
          alias ssh = kitten ssh
          $env.config = {
            edit_mode: vi
            shell_integration: true

            keybindings: [
            { name: evie_down
              modifier: control
              keycode: char_j
              mode: vi_insert
              event: { send: MenuNext }
            }
            { name: evie_u
              modifier: control
              keycode: char_k
              mode: vi_insert
              event: { send: MenuPrevious }
            }]
          }
        '';
      };

      carapace = {
        enable = true;
        enableBashIntegration = true;
        enableFishIntegration = true;
        enableNushellIntegration = true;
      };

      atuin = {
        enable = true;
        enableBashIntegration = true;
        enableFishIntegration = true;
        enableNushellIntegration = true;
        flags = [ "--disable-up-arrow" ];
      };

      fd = {
        enable = true;
        hidden = true;
      };

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
  };
}
