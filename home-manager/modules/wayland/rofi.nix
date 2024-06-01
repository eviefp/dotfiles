{ pkgs, ... }:
{
  imports = [ ];

  config = {
    home.packages = [
      pkgs.tessen
    ];
    programs.rofi = {
      enable = true;
      cycle = true;
      package = pkgs.rofi-wayland;
      pass = {
        enable = true;
      };
      plugins = [
        pkgs.rofi-pass-wayland
      ];
      # theme = "purple";
      theme = ./../../../config/rofi-rounded-common.rasi;
      extraConfig = {
        show-icons = true;
        terminal = "kitty";
        sidebar-mode = true;

        kb-mode-next = "Control+l";
        kb-mode-previous = "Control+h";
        kb-row-up = "Control+k";
        kb-row-down = "Control+j";
        kb-remove-to-eol = "";
        kb-accept-entry = "Return";
        kb-mode-complete = "";
        kb-remove-char-back = "BackSpace";
      };
    };
  };
}
