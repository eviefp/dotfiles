{pkgs, ...}: {
  config.programs.nixvim = {
    extraPlugins = [
      (pkgs.vimUtils.buildVimPlugin {
        pname = "time-machine.nvim";
        version = "v1.5.4";
        src = pkgs.fetchFromGitHub {
          owner = "y3owk1n";
          repo = "time-machine.nvim";
          rev = "6657a7db8a07ecfa8a266be34bebfb6827bb2eab";
          hash = "sha256-8KRWE0eNPSBh4HT6sDix9hz6QWcVk3Ne1bjmb/NDPio=";
        };
      }) # https://github.com/y3owk1n/time-machine.nvim
    ];

    extraConfigLua = ''
      require('time-machine').setup {
        diff_tool = 'delta',
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
