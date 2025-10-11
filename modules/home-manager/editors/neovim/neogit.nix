{
  lib,
  config,
  ...
}: let
  cfg = config.evie.editors.neovim;
in {
  config = lib.mkIf cfg.enable {
    home.packages = [
    ];
    programs.nixvim = {
      keymaps = [
        {
          key = "<leader>gs";
          action = "<cmd>Neogit cwd=%:p:h<cr>";
          options.desc = "neogit UI";
        }
      ];
      plugins.neogit = {
        enable = true;

        settings = {
          graph_style = "kitty";
          process_spinner = true;
          disable_insert_on_commit = true;
          integrations = {
            telescope = true;
            diffview = false;
          };
        };
      };
    };
  };
}
