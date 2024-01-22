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

        programs = {


          zathura = {
            enable = true;
            extraConfig = ''
              set notification-error-bg       "#1d2021" # bg
              set notification-error-fg       "#fb4934" # bright:red
              set notification-warning-bg     "#1d2021" # bg
              set notification-warning-fg     "#fabd2f" # bright:yellow
              set notification-bg             "#1d2021" # bg
              set notification-fg             "#b8bb26" # bright:green

              set completion-bg               "#504945" # bg2
              set completion-fg               "#ebdbb2" # fg
              set completion-group-bg         "#3c3836" # bg1
              set completion-group-fg         "#928374" # gray
              set completion-highlight-bg     "#83a598" # bright:blue
              set completion-highlight-fg     "#504945" # bg2

              # Define the color in index mode
              set index-bg                    "#504945" # bg2
              set index-fg                    "#ebdbb2" # fg
              set index-active-bg             "#83a598" # bright:blue
              set index-active-fg             "#504945" # bg2

              set inputbar-bg                 "#1d2021" # bg
              set inputbar-fg                 "#ebdbb2" # fg

              set statusbar-bg                "#504945" # bg2
              set statusbar-fg                "#ebdbb2" # fg

              set highlight-color             "#fabd2f" # bright:yellow
              set highlight-active-color      "#fe8019" # bright:orange

              set default-bg                  "#1d2021" # bg
              set default-fg                  "#ebdbb2" # fg
              set render-loading              true
              set render-loading-bg           "#1d2021" # bg
              set render-loading-fg           "#ebdbb2" # fg

              # Recolor book content's color
              set recolor-lightcolor          "#1d2021" # bg
              set recolor-darkcolor           "#ebdbb2" # fg
              set recolor                     "true"
              set recolor-keephue             true      # keep original color
            '';
          };
        };
      })
      (lib.mkIf cfg.latex {
        home.packages = [ pkgs.tectonic pkgs.texlab ];
      })
    ];
}
