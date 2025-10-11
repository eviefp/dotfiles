{pkgs, ...}: {
  config = {
    home.packages = [
      pkgs.mermaid-cli
      pkgs.plantuml
      pkgs.d2
      pkgs.gnuplot
    ];

    programs.nixvim.plugins = {
      diagram = {
        enable = true;
      };

      # Needed by 'diagram'.
      image = {
        enable = true;
        settings = {
          backend = "kitty";
        };
      };
    };
  };
}
