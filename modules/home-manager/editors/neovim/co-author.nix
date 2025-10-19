{pkgs, ...}: {
  config.programs.nixvim = {
    extraPlugins = [
      (pkgs.vimUtils.buildVimPlugin {
        pname = "co-author.nvim";
        version = "v1.0";
        src = pkgs.fetchFromGitHub {
          owner = "2KAbhishek";
          repo = "co-author.nvim";
          rev = "f07e2ec7b4267c1c38cecf5f209c0844373bc1cc";
          hash = "sha256-YP5LaXvizPWIEpZOfXUrxAFG2K3oD9sVeXHJQj6vNzo=";
        };
      }) # https://github.com/2KAbhishek/co-author.nvim
    ];
  };
}
