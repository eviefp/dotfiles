/**
**************************************************************************
* presenterm
*************************************************************************
*/
{
  lib,
  config,
  pkgs,
  ...
}: let
  cfg = config.evie.term.presenterm;
  settingsFormat = pkgs.formats.yaml {};
in {
  options.evie.term.presenterm = {
    enable = lib.mkEnableOption "enable presenterm";
    settings = lib.mkOption {
      type = settingsFormat.type;
      default = {
        defaults = {
          terminal_font_size = 16;
          theme = "terminal-dark";
          image_protocol = "kitty-local";
        };

        typst.ppi = 300;
        d2.scale = 2;
        mermaid.scale = 2;

        options = {
          implicit_slide_ends = false;
          command_prefix = "";
          incremental_lists = false;
          strict_front_matter_parsing = false;
          end_slide_shorthand = false;
        };

        snippet = {
          exec.enable = true;
          exec_replace.enable = true;
          render.threads = 2;
        };

        speaker_notes = {
          listen_address = "127.0.0.1:58418";
          publish_address = "127.0.0.1:59418";
        };

        bindings = {
          next = ["l" "j" "<right>" "<down>" " "];
          next_fast = ["n"];
          previous = ["h" "k" "<left>" "<up>"];
          previous_fast = ["p"];
          first_slide = ["gg"];
          last_slide = ["G"];
          go_to_slide = ["<number>G"];
          execute_code = ["<c-e>"];
          reload = ["<c-r>"];
          toggle_slide_index = ["<c-i>"];
          toggle_bindings = ["?"];
          close_modal = ["<esc>"];
          exit = ["<c-c>" "q"];
          suspend = ["<c-z>"];
          skip_pauses = ["s"];
        };

        transition = {
          animation.style = "slide_horizontal";
          duration_millis = 1000;
          frames = 60;
        };
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.presenterm
    ];

    xdg.configFile."presenterm/config.yaml".source = settingsFormat.generate "config.yaml" cfg.settings;
  };
}
