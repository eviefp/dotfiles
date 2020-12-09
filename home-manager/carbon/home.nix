let
  common = import ../common.nix;
  emacs = import ../emacs.nix {
    pkgs = common.nixpkgs;
    locals = ./locals.el;
  };

in
{
  nixpkgs.config.nixpkgs.config.allowUnfree = true;

  accounts = common.accounts;

  home.packages =
    common.packages.generic
    ++ common.packages.nixos
    ++ common.packages.programming
    ++ common.packages.haskell
    ++ common.packages.provers
    ++ common.packages.scala
    ++ common.packages.latex
    ++ common.packages.streaming;

    home.sessionVariables = common.sessionVariables;
    home.file = common.file // emacs.file // {
      ".xmobarrc".source = ../../config/carbon/xmobarrc;
      ".xmonad/xmonad.hs".source = ../../config/xmonad/xmonad.hs;
      ".xmonad/get-mails.sh".source = ../../config/xmonad/get-mails.sh;
      ".xmonad/get-mic.sh".source = ../../config/xmonad/get-mic.sh;
      ".xmonad/get-volume.sh".source = ../../config/xmonad/get-volume.sh;
      ".config/fish/functions/fixUI.fish".source = ../../config/fish/functions/fixUI.fish;
      ".config/fish/functions/m0.fish".source = ../../config/fish/functions/m0.fish;
      ".config/fish/functions/m1.fish".source = ../../config/fish/functions/m1.fish;
      ".config/fish/functions/m2.fish".source = ../../config/fish/functions/m2.fish;
      ".config/fish/functions/rt.fish".source = ../../config/fish/functions/rt.fish;
      ".config/fish/functions/ssh.fish".source = ../../config/fish/functions/ssh.fish;
    };

    programs = common.programs // common.helpers.mkKitty {
      "font_size" = "12.0";
    };

    # This does not currently work because package is non-definable.
    # Need to update home-manager, I think.
    # services = common.services // {
    #   emacs = {
    #     enable = true;
    #     package = emacs;
    #     client.enable = true;
    #   };
    # };
}
