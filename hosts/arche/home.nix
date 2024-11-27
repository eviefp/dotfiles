{ dotfiles, pkgs, ... }:
{
  imports = with dotfiles.self.homeManagerModules; [
    common
  ];

  config = {
    evie = {
      common.enable = true;

      system = {
        gpg.enable = true;
        home-manager = {
          user = "every";
        };
      };

      wayland = {
        enable = true;
      };
    };

    home.packages = [
      pkgs.chromium
    ];
  };
}
