{pkgs, ...}: {
  config = {
    home.packages = [
      pkgs.ast-grep
    ];

    programs.nixvim.plugins.grug-far = {
      enable = true;
      settings = {
        debounceMs = 1000;
        minSearchChars = 1;
        maxSearchMatches = 2000;
        normalModeSearch = false;
        maxWorkers = 8;
        engine = "ripgrep";
        engines = {
          ripgrep = {
            path = "rg";
            showReplaceDiff = true;
          };
        };
      };
    };
  };
}
