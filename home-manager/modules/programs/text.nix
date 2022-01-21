/****************************************************************************
  * programs/text module
  *
  * Enable packages I use for text-related development:
  *   - 'mailcap' for reading MIME emails in emacs
  *   - 'ispell' for spelling
  *   - 'pandoc' for document formatting
  *   - 'tectonic' for building latex packages
  *   - 'texlab' for I'm not sure what
  **************************************************************************/
{ lib, config, pkgs, ... }:
let cfg = config.evie.programs.text;
in
{
  imports = [ ];

  options.evie.programs.text = {
    enable = lib.options.mkEnableOption "Enable text";
    # This can become a separate module if it becomes more intricate.
    latex = lib.options.mkEnableOption "Enable latex packages.";
  };

  config = lib.mkMerge
    [
      (lib.mkIf cfg.enable {
        home.packages = [
          pkgs.mailcap
          pkgs.ispell
          pkgs.pandoc
        ];
      })
      (lib.mkIf cfg.latex {
        home.packages = [ pkgs.tectonic pkgs.texlab ];
      })
    ];
}
