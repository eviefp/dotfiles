{pkgs, ...}: {
  config.programs.nixvim = {
    extraPlugins = [
      (pkgs.vimUtils.buildVimPlugin {
        pname = "co-author.nvim";
        version = "v1.0";
        src = pkgs.fetchFromGitHub {
          owner = "2KAbhishek";
          repo = "co-author.nvim";
          rev = "2f012714247dfe1959ba53fa50e4b1320d86d1b8";
          hash = "sha256-5/UORMt9TxOM7LRDKSbRymBt11XPe3OWN/8rwy0IkZg=";
        };
      }) # https://github.com/2KAbhishek/co-author.nvim
    ];
  };
}
