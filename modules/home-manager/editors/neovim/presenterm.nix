{pkgs, ...}: {
  config.programs.nixvim = {
    extraPlugins = [
      (pkgs.vimUtils.buildVimPlugin {
        pname = "presenterm.nvim";
        version = "v1.4.0";
        src = pkgs.fetchFromGitHub {
          owner = "Piotr1215";
          repo = "presenterm.nvim";
          rev = "610ad9e44abbcff3fe41c71f14e42622b4f9a6f7";
          hash = "sha256-Hw6Lw1CfJcrlMuCi16WHior6EeJqXJ7S+aMfIg29HWk=";
        };
      }) # https://github.com/Piotr1215/presenterm.nvim
    ];

    extraConfigLua = ''
      require('presenterm').setup {
        slide_marker = '---',
        default_keybindings = true,
        picker = {
          provider = 'telescope',
        },
        preview = {
          command = 'presenterm',
          presentation_preview_sync = true,
          login_shell = false,
        },
      }
    '';

    keymaps = [
      {
        key = "<leader>tm";
        action = "<cmd>TimeMachineToggle<cr>";
        options.desc = "Time Machine: toggle tree";
      }
      {
        key = "<leader>tx";
        action = "<cmd>TimeMachinePurgeCurrent<cr>";
        options.desc = "Time Machine: purge current";
      }
      {
        key = "<leader>tX";
        action = "<cmd>TimeMachinePurgeAll<cr>";
        options.desc = "Time Machine: purge ALL";
      }
      {
        key = "<leader>tl";
        action = "<cmd>TimeMachineLogShow<cr>";
        options.desc = "Time Machine: show logs";
      }
    ];
  };
}
