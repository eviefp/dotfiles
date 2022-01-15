let
  common = import ../common.nix;
  emacs = import ../emacs.nix {
    pkgs = common.nixpkgs;
    locals = ./locals.el;
  };
  email = import ../email.nix { pkgs = common.nixpkgs; };
in
{
  nixpkgs.config.nixpkgs.config.allowUnfree = true;

  accounts.email = {
    maildirBasePath = "/home/evie/mail";
    accounts = {
      eevie = email.eevie;
    };
  };

  home.packages =
    common.packages.generic
    ++ [ emacs.derivation ]
    ++ common.packages.nixos
    ++ common.packages.programming
    ++ common.packages.haskell
    ++ common.packages.provers
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
      ".config/fish/functions/ec.fish".source = ../../config/fish/functions/ec.fish;
      ".config/fish/functions/ed.fish".source = ../../config/fish/functions/ed.fish;
    };

    programs = common.programs // common.helpers.mkKitty {
      "font_size" = "10.0";
    } // {
      neomutt = {
        enable = true;
        vimKeys = true;
        sort = "threads";
        extraConfig = ''
          set sort_aux = reverse-last-date-received
        '';
      };
      mbsync = {
        enable = true;
      };
      msmtp = {
        enable = true;
      };
      notmuch = {
        enable = true;
        hooks = {
          preNew = "mbsync --all";
        };
      };
    };

    fonts = common.fonts;

    services = common.services // {
      mbsync.enable = true;

      # emacs = {
      #   enable = true;
      #   package = emacs.derivation;
      #   client = {
      #     enable = true;
      #     arguments = [ "-c" ];
      #   };
      #   socketActivation.enable = true;
      # };
    };
}
