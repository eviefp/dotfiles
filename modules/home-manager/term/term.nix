/****************************************************************************
  * programs/shell module
  *
  **************************************************************************/
{ dotfiles, lib, config, pkgs, ... }:
let
  cfg = config.evie.term;
  scripts = {
    crypto = pkgs.concatScript "crypto" [ ../../../scripts/crypto.nu ];
  };
  exp = pkgs.writeShellScriptBin "exp" ''
    echo 'cat foo | choose 2 1:-1'
    echo 'sd find replace file'
    echo 'dust -- du, but nicer'
    echo 'gping [hosts..]'
    echo 'sudo below live -- todo: config'
    echo 'yazi -- file term'
    echo 'bandwhich -- bandwith info'
    echo 'systemctl-tui'
    echo 'serie -- pretty git log graph'
    echo 'rainfrog --url <PGCONN> --driver postgres -- pretty git log graph'
    echo 'gpg-tui -- gpg tui'
    echo 'thokr -- typing tui'
    echo 'impala -- wifi tui'
    echo 'bluetui -- bluetooth tui'
    echo 'diskonaut -- disk space cleaner tui'
    echo 'confetty'
  '';
in
{
  options.evie.term = {
    enable = lib.mkEnableOption "term defaults";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.killall
      pkgs.ripgrep
      pkgs.wget
      pkgs.unzip
      pkgs.unrar
      pkgs.zip
      pkgs.sops
      dotfiles.porc.packages.${pkgs.system}.default
      pkgs.nvd
      pkgs.calc

      # experiments
      pkgs.choose
      pkgs.du-dust
      pkgs.sd
      pkgs.gping
      pkgs.below
      pkgs.ffmpegthumbnailer
      pkgs.nix-tree
      pkgs.bandwhich
      pkgs.confetty
      pkgs.systemctl-tui
      pkgs.serie
      pkgs.rainfrog
      pkgs.thokr
      pkgs.gpg-tui
      pkgs.impala
      pkgs.bluetui
      pkgs.diskonaut
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
          key = "me@evie.ro";
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

      nushell = {
        enable = true;
        package = pkgs.nushell;
        shellAliases = {
          kssh = "kitten ssh";
          gco = "git checkout";
          gf = "git fetch";
          gb = "git br";
          vi = "nvim";
        };
        environmentVariables = {
          KEYID_EVIE = "6A9BDD4C9EE01C020EDD1F6E272D83521C488CCD";
        };
        configFile.text = ''
          use ${scripts.crypto} *
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
        enableBashIntegration = true;
        enableFishIntegration = true;
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

      yazi = {
        enable = true;
        enableBashIntegration = true;
        enableFishIntegration = true;
        enableNushellIntegration = true;
        settings = {
          manager = {
            sort_by = "natural";
            show_hidden = true;
          };
        };
        # keymap = {
        #   manager.keymap = [
        #     { run = "escape"; on = "<Esc>"; }
        #     { run = "quit"; on = "q"; }
        #
        #     { run = "arrow -1"; on = "k"; }
        #     { run = "arrow 1"; on = "j"; }
        #
        #     { run = "arrow -50%"; on = "<C-u>"; }
        #     { run = "arrow 50%"; on = "<C-d>"; }
        #
        #     { run = "arrow -99999999"; on = [ "g" "g" ]; }
        #     { run = "arrow 99999999"; on = "G"; }
        #
        #     { run = "leave"; on = "h"; }
        #     { run = "enter"; on = "l"; }
        #
        #     { run = [ "toggle" "arrow 1" ]; on = "<Space>"; }
        #     { run = "visual_mode"; on = "v"; }
        #     { run = "visual_mode --unset"; on = "V"; }
        #
        #     { run = "seek -5"; on = "K"; }
        #     { run = "seek 5"; on = "J"; }
        #
        #     { run = "open"; on = "o"; }
        #     { run = "open --interactive"; on = "O"; }
        #   ];
        # };
      };

      joshuto = {
        enable = false;
        settings = {
          xdg_open = true;
          use_trash = false;
          max_preview_size = 10000000;
          display = {
            show_hidden = true;
          };
          preview = {
            preview_protocol = "kitty";
            use_xdg_thumbs = true;
            xdg_thumb_size = "xxlarge";
          };
        };
      };

    };
  };
}
