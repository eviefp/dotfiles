{ lib, config, pkgs, ... }:
let
  cfg = config.evie.term;
  scripts = {
    crypto = pkgs.concatScript "crypto" [ ../../../scripts/crypto.nu ];
    misc = pkgs.concatScript "misc" [ ../../../scripts/misc.nu ];
  };
in
{
  options.evie.term = {
    enable = lib.mkEnableOption "term defaults";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.dig
      pkgs.killall
      pkgs.nvd
      pkgs.ripgrep
      pkgs.sd
      pkgs.sops
      pkgs.unrar
      pkgs.unzip
      pkgs.wget
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
        icons = "auto";
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
          key = "59FDB6CD4C63F6B2";
          signByDefault = true;
        };
        difftastic = {
          enable = true;
          color = "auto";
          background = "dark";
          # display = "side-by-side";
        };
        aliases = {
          ll = "log --graph --decorate --oneline --abbrev-commit";
          lola = "log --graph --decorate --oneline --abbrev-commit --all";
          hist =
            "log --pretty=format:'%h %ad | %s%d [%an]' --graph --date=short";
          lol =
            "log --color --graph --pretty=format:'%Cred%h -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --";
          br =
            "for-each-ref --sort=committerdate refs/heads/ --format='%(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(color:red)%(objectname:short)%(color:reset) - %(contents:subject) - %(authorname) (%(color:green)%(committerdate:relative)%(color:reset))'";
          work = "log --pretty=format:'%h%x09%an%x09%ad%x09%s'";
        };
        extraConfig = {
          init.defaultBranch = "main";
          pull.ff = "only";
          merge.conflictstyle = "diff3";
        };
        ignores = [ "TAGS" ];
        userEmail = "me@evie.ro";
        userName = "Evie Ciobanu";
      };

      jujutsu = {
        enable = false;
        settings = {
          user = {
            email = "me@evie.ro";
            name = "Evie Ciobanu";
          };
        };
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

      starship = {
        enable = true;
        enableFishIntegration = true;
        enableNushellIntegration = true;
        settings = {
          add_newline = true;

          character = {
            success_symbol = "[☭](bold green)";
            error_symbol = "[✗](bold red)";
            vicmd_symbol = "[☭](bold yellow)";
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
            style = "bold fg:#d62dc9";
          };

          shell = {
            disabled = true;
            fish_indicator = "";
            bash_indicator = "󱆃";
            nu_indicator = "";
          };

          username = {
            style_user = "bold fg:#d62dc9";
            show_always = true;
            format = "[$user]($style)[@](fg:#b17361)";
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

      nushell = {
        enable = true;
        package = pkgs.nushell;
        shellAliases = {
          kssh = "kitten ssh";
          gco = "git checkout";
          gf = "git fetch";
          gb = "git br";
          gs = "git status";
          vi = "nvim";
          ki = "khal interactive";
          calm = "cal --week-start mo --month-names";
          caly = "cal --week-start mo --month-names --full-year 2025"; # todo: maybe make this a script instead
        };
        environmentVariables = {
          KEYID_EVIE = "6A9BDD4C9EE01C020EDD1F6E272D83521C488CCD";
          EDITOR = "nvim";
          DIRENV_WARN_TIMEOUT = "0";
        };
        configFile.text = ''
          use ${scripts.crypto} *
          use ${scripts.misc} *
          $env.config = {
            edit_mode: vi
            shell_integration: {
              osc2: true
              osc7: true
              osc8: true
              osc9_9: false
              osc133: true
              osc633: true
              reset_application_mode: true
            }
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
        enableFishIntegration = false;
        enableNushellIntegration = true;
      };

      atuin = {
        enable = true;
        enableBashIntegration = true;
        enableFishIntegration = true;
        enableNushellIntegration = true;
        flags = [ ];
        settings = {
          filter_mode_shell_up_key_binding = "session";
        };
      };

      fd = {
        enable = true;
        hidden = true;
      };

    };
  };
}
