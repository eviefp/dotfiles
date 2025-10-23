{...}: {
  config.programs.nixvim = {
    autoCmd = [
      {
        event = ["BufEnter"];
        pattern = ["*.commie"];
        command = ":setlocal filetype=lean";
      }
    ];
  };
}
