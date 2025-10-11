{pkgs, ...}: {
  config.programs.nixvim = {
    extraPlugins = [
      (pkgs.vimUtils.buildVimPlugin {
        pname = "presenterm.nvim";
        version = "v1.4.0";
        src = pkgs.fetchFromGitHub {
          owner = "Piotr1215";
          repo = "presenterm.nvim";
          rev = "9918cf87094be527af97ea5a8bd69f7c30c62a3e";
          hash = "sha256-fpBj2dO437LW+cdWs8wg2GYJoZer5XBWq9BL4e2R/OM=";
        };
        patches = [
          # https://github.com/Piotr1215/presenterm.nvim/pull/4
          ../../../../patches/presenterm-nvim-use-slide-marker.patch
        ];
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
