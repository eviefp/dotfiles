{ config, lib, dotfiles, pkgs, ... }:
let
  cfg = config.evie.term.tui;
in
{
  options.evie.term.tui = {
    enable = lib.mkEnableOption "term tui";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      dotfiles.gitu.packages.${pkgs.system}.default
      dotfiles.porc.packages.${pkgs.system}.default
      pkgs.calc
    ];

    programs = {
      bottom = {
        enable = true;
        settings = {
          left_legend = true;
          temperature_type = "c";
          color = "gruvbox";
        };
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

    };

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

  };
}
