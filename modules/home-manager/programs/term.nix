/****************************************************************************
  * programs/shell module
  *
  **************************************************************************/
{ dotfiles, pkgs, ... }:
let
  exp = pkgs.writeShellScriptBin "exp" ''
    echo 'cat foo | choose 2 1:-1'
    echo 'sd find replace file'
    echo 'atuin - cmd find?'
    echo gping [hosts..]
    echo 'sudo below live -- todo: config'
    echo 'yazi'
  '';
in
{
  config = {
    home.packages = [
      pkgs.killall
      pkgs.ripgrep
      pkgs.wget
      pkgs.unzip
      pkgs.zip
      pkgs.sops
      dotfiles.gitu.packages.${pkgs.system}.default
      pkgs.sysz
      dotfiles.porc.packages.${pkgs.system}.default
      pkgs.nvd

      # experiments
      pkgs.choose
      pkgs.du-dust
      pkgs.sd
      pkgs.gping
      pkgs.below

      pkgs.ffmpegthumbnailer
      pkgs.unrar

      pkgs.nix-tree

      pkgs.calc

      exp
    ];

    home.file.".config/gitu/config.toml".text = ''

# This file contains Gitu's default configuration.
# It is possible to override settings with an equivalent file at:
# `~/.config/gitu/config.toml`

[general]
always_show_help.enabled = false
confirm_quit.enabled = false

[style]
# fg / bg can be either of:
# - a hex value: "#707070"
# - an ansi color name: "light blue"
# - an ansi color index: "255"
# - "reset" will set the terminal's default foreground / background color.

# 'mods' can be any combination of (multiple values separated by '|'):
# "BOLD|DIM|ITALIC|UNDERLINED|SLOW_BLINK|RAPID_BLINK|REVERSED|HIDDEN|CROSSED_OUT"

# Example style config values:
# section_header = { fg = "#808080" }
# section_header = { bg = "light green", mods = "UNDERLINED|ITALIC" }

section_header = { fg = "yellow" }
file_header = { fg = "magenta" }
hunk_header = { fg = "blue" }

diff_highlight.tag_old = { fg = "red", mods = "BOLD" }
diff_highlight.tag_new = { fg = "green", mods = "BOLD" }
diff_highlight.unchanged_old = { mods = "DIM" }
diff_highlight.unchanged_new = { mods = "DIM" }
diff_highlight.changed_old = { fg = "red" }
diff_highlight.changed_new = { fg = "green"}

syntax_highlight.enabled = true
syntax_highlight.attribute = { fg = "yellow" }
syntax_highlight.comment = { fg = "gray" }
syntax_highlight.constant_builtin = {}
syntax_highlight.constant = {}
syntax_highlight.constructor = {}
syntax_highlight.embedded = {}
syntax_highlight.function_builtin = { fg = "cyan" }
syntax_highlight.function = { fg = "blue" }
syntax_highlight.keyword = { fg = "magenta" }
syntax_highlight.number = {}
syntax_highlight.module = { fg = "cyan" }
syntax_highlight.property = {}
syntax_highlight.operator = {}
syntax_highlight.punctuation_bracket = {}
syntax_highlight.punctuation_delimiter = {}
syntax_highlight.string_special = { fg = "yellow" }
syntax_highlight.string = { fg = "yellow" }
syntax_highlight.tag = {}
syntax_highlight.type = { fg = "yellow" }
syntax_highlight.type_builtin = { fg = "yellow" }
syntax_highlight.variable_builtin = {}
syntax_highlight.variable_parameter = {}

cursor = { fg = "blue" }
selection_bar = { fg = "blue", mods = "DIM" }
selection_line = { mods = "BOLD" }
# You may want to set `selection_area.bg` to a nice background color.
# Looks horrible with regular terminal colors, so is therefore not set.
selection_area = {}

hash = { fg = "yellow" }
branch = { fg = "green" }
remote = { fg = "red" }
tag = { fg = "yellow" }

command = { fg = "blue", mods = "BOLD" }
active_arg = { fg = "light red", mods = "BOLD" }
hotkey = { fg = "magenta" }

[bindings]
root.quit = ["q", "<esc>"]
root.refresh = ["g"]
root.toggle_section = ["<tab>"]
root.move_up = ["k", "<up>"]
root.move_down = ["j", "<down>"]
root.move_up_line = ["<ctrl+k>", "<ctrl+up>"]
root.move_down_line = ["<ctrl+j>", "<ctrl+down>"]
root.move_prev_section = ["<alt+k>", "<alt+up>"]
root.move_next_section = ["<alt+j>", "<alt+down>"]
root.move_parent_section = ["<alt+h>", "<alt+left>"]
root.half_page_up = ["<ctrl+u>"]
root.half_page_down = ["<ctrl+d>"]
root.show_refs = ["Y"]
root.show = ["<enter>"]
root.discard = ["K"]
root.stage = ["s"]
root.unstage = ["u"]
root.copy_hash = ["y"]

root.help_menu = ["h"]
help_menu.quit = ["q", "<esc>"]

root.branch_menu = ["b"]
branch_menu.checkout = ["b"]
branch_menu.checkout_new_branch = ["c"]
branch_menu.quit = ["q", "<esc>"]

root.commit_menu = ["c"]
commit_menu.--all = ["-a"]
commit_menu.--allow-empty = ["-e"]
commit_menu.--verbose = ["-v"]
commit_menu.--no-verify = ["-n"]
commit_menu.--reset-author = ["-R"]
commit_menu.--signoff = ["-s"]
commit_menu.commit = ["c"]
commit_menu.commit_amend = ["a"]
commit_menu.commit_fixup = ["f"]
commit_menu.quit = ["q", "<esc>"]

root.fetch_menu = ["f"]
fetch_menu.--prune = ["-p"]
fetch_menu.--tags = ["-t"]
fetch_menu.fetch_all = ["a"]
fetch_menu.quit = ["q", "<esc>"]
fetch_menu.fetch_elsewhere = ["e"]

root.log_menu = ["l"]
log_menu.log_current = ["l"]
log_menu.log_other = ["o"]
log_menu.quit = ["q", "<esc>"]
log_menu.-n = ["-n"]
log_menu.--grep = ["-F"]

root.pull_menu = ["F"]
pull_menu.--rebase = ["-r"]
pull_menu.pull = ["p"]
pull_menu.pull_elsewhere = ["e"]
pull_menu.quit = ["q", "<esc>"]

root.push_menu = ["P"]
push_menu.--force-with-lease = ["-f"]
push_menu.--force = ["-F"]
push_menu.--no-verify = ["-h"]
push_menu.--dry-run = ["-n"]
push_menu.push = ["p"]
push_menu.push_elsewhere = ["e"]
push_menu.quit = ["q", "<esc>"]

root.rebase_menu = ["r"]
rebase_menu.--keep-empty = ["-k"]
rebase_menu.--preserve-merges = ["-p"]
rebase_menu.--committer-date-is-author-date = ["-d"]
rebase_menu.--autosquash = ["-a"]
rebase_menu.--autostash = ["-A"]
rebase_menu.--interactive = ["-i"]
rebase_menu.--no-verify = ["-h"]
rebase_menu.rebase_interactive = ["i"]
rebase_menu.rebase_abort = ["a"]
rebase_menu.rebase_continue = ["c"]
rebase_menu.rebase_elsewhere = ["e"]
rebase_menu.rebase_autosquash = ["f"]
rebase_menu.quit = ["q", "<esc>"]

root.reset_menu = ["X"]
reset_menu.reset_soft = ["s"]
reset_menu.reset_mixed = ["m"]
reset_menu.reset_hard = ["h"]
reset_menu.quit = ["q", "<esc>"]

root.revert_menu = ["V"]
revert_menu.--edit = ["-e"]
revert_menu.--no-edit = ["-E"]
revert_menu.--signoff = ["-s"]
revert_menu.revert_abort = ["a"]
revert_menu.revert_continue = ["c"]
revert_menu.revert_commit = ["V"]
revert_menu.quit = ["q", "<esc>"]

root.stash_menu = ["z"]
stash_menu.--all = ["-a"]
stash_menu.--include-untracked = ["-u"]
stash_menu.stash = ["z"]
stash_menu.stash_index = ["i"]
stash_menu.stash_worktree = ["w"]
stash_menu.stash_keep_index = ["x"]
stash_menu.stash_pop = ["p"]
stash_menu.stash_apply = ["a"]
stash_menu.stash_drop = ["k"]
stash_menu.quit = ["q", "<esc>"]
    '';

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
        userEmail = "me@eevie.ro";
        userName = "Evie Ciobanu";
      };

      jujutsu = {
        enable = true;
        settings = {
          user = {
            email = "me@eeie.ro";
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
        configFile.text = ''
          alias ssh = kitten ssh
          alias gco = git checkout
          alias gf = git fetch
          alias gb = git br
          alias sw = git switch
          alias gp = git pull
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
        settings = { };
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
