{pkgs, ...}: {
  config.programs.nixvim = {
    extraPlugins = [
      (pkgs.vimUtils.buildVimPlugin {
        pname = "git-conflict";
        version = "v1.1";
        src = pkgs.fetchFromGitHub {
          owner = "eviefp";
          repo = "git-conflict.nvim";
          rev = "a4d062373f98675c268df385a02390df71be1e6c";
          hash = "sha256-yAON9F+qlfD4gpvOg7SRrba3JN707tdYElvJ9uLBPt4=";
        };
      }) # https://github.com/eviefp/git-conflict.nvim
    ];

    keymaps = [
    ];

    extraConfigLua = ''
      require('git-conflict').setup {
        default_mappings = true,
        default_commands = true,
        disable_diagnostics = false,
        list_opener = 'copen',
        highlights = {
          incoming = 'DiffAdd',
          current = 'DiffText',
        }
      }
    '';
  };
}
