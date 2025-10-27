{pkgs, ...}: {
  config.programs.nixvim = {
    extraPlugins = [
      (pkgs.vimUtils.buildVimPlugin {
        pname = "blame.nvim";
        version = "v1.0";
        src = pkgs.fetchFromGitHub {
          owner = "FabijanZulj";
          repo = "blame.nvim";
          rev = "f3f6153ea94e1120f2776f22bbbd63c4aeebaf32";
          hash = "sha256-CY768BvqU8zJ2qZKVC9s1lLctBzam1GeZD7UnD7nBBc=";
        };
      }) # https://github.com/FabijanZulj/blame.nvim
    ];

    keymaps = [
      {
        key = "<leader>gb";
        action = "<cmd>BlameToggle<cr>";
        options.desc = "blame toggle";
      }
    ];

    extraConfigLua = ''
      require('blame').setup {}
    '';
  };
}
