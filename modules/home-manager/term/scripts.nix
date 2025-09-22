{lib, ...}: {
  options.evie.term.scripts = {
    enable = lib.mkEnableOption "scripts";
  };

  config.evie.term.scripts = {};
}
