{...}: {
  config.programs.nixvim = {
    autoCmd = [
      {
        event = ["BufEnter"];
        pattern = ["*.el"];
        command = ":setlocal filetype=lisp";
      }
    ];

    plugins = {
      orgmode.enable = true;
    };
  };
}
