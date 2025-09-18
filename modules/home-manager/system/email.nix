/****************************************************************************
  * Email module
  *
  * Set up email account, imap sync, notmuch mail management.
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.system.email;
in
{
  options.evie.system.email = {
    enable = lib.mkEnableOption "calendar defaults";
  };

  config = lib.mkIf cfg.enable {
    accounts.email = {

      maildirBasePath = "/home/evie/mail";

      accounts = {
        eevie = {
          primary = true;
          address = "me@eevie.ro";
          userName = "evieciobanu..thelxinoe";
          realName = "Evie Ciobanu";

          gpg = {
            key = "6A9BDD4C9EE01C020EDD1F6E272D83521C488CCD";
            signByDefault = false;
          };

          signature = {
            text = ''

              -- Evie
            '';
            showSignature = "append";
          };

          passwordCommand = "${pkgs.coreutils}/bin/cat /run/secrets/peroxide_thelxinoe";

          imap = {
            host = "thelxinoe";
            port = 1143;
            tls = {
              enable = true;
              useStartTls = true;
              certificatesFile = "/run/secrets/evie_certificate";
            };
          };

          smtp = {
            host = "thelxinoe";
            port = 1025;
            tls = {
              enable = true;
              useStartTls = true;
              certificatesFile = "/run/secrets/evie_certificate";
            };
          };

          mbsync = {
            enable = true;
            create = "both";
            remove = "both";
          };

          msmtp = {
            enable = true;
            tls.fingerprint = "BC:74:78:C0:F2:33:58:9E:D4:7F:03:D9:D4:48:97:E4:12:AB:FC:B9:A7:02:A6:A7:5B:A8:33:CB:42:C0:B9:71";
          };

          notmuch.enable = true;
        };

        gmail-primary = {
          primary = false;
          address = "alexa.eviest@gmail.com";
          userName = "alexa.eviest@gmail.com";
          realName = "Evie Ciobanu";

          gpg = {
            key = "6A9BDD4C9EE01C020EDD1F6E272D83521C488CCD";
            signByDefault = false;
          };

          signature = {
            text = ''

              -- Evie
            '';
            showSignature = "append";
          };

          passwordCommand = "${pkgs.coreutils}/bin/cat /run/secrets/gmail_password";

          imap = {
            host = "imap.gmail.com";
            port = 993;
            tls = {
              enable = true;
              # useStartTls = true;
            };
          };

          smtp = {
            host = "smtp.gmail.com";
            port = 587;
            tls = {
              enable = true;
              useStartTls = true;
            };
          };

          mbsync = {
            enable = true;
            create = "both";
            remove = "both";
          };

          msmtp = {
            enable = true;
          };

          notmuch.enable = true;
        };
      };
    };

    home.file.".mailcap".text = ''
      text/html;  w3m -dump -o document_charset=%{charset} '%s'; nametemplate=%s.html; copiousoutput
    '';

    programs = {
      mbsync = {
        enable = true;
      };
      msmtp = {
        enable = true;
      };
      notmuch = {
        enable = true;
        new.tags = [ "unread" "unsorted" "inbox" ];
        hooks = {
          postNew = ''
            notmuch tag +evie -unsorted -- to:*@eevie.ro
            notmuch tag +evie -unsorted -- to:me@eevie.ro
            notmuch tag +evie -unsorted -- to:'Evie Ciobanu'
            notmuch tag +meowing -unsorted -- to:*@group-meowing.ro
            notmuch tag +meowing -unsorted -- from:*@noreply@codeberg.org
            notmuch tag +gmail -unsorted -- to:alexaeviest@gmail.com
            notmuch tag +gmail -unsorted -- to:alexa.eviest@gmail.com
            notmuch tag +hf -unsorted -- from:*@haskell.foundation
            notmuch tag +hf -unsorted -- to:*@haskell.foundation
            notmuch tag +sent -unsorted -- from:*@eevie.ro
            notmuch tag --remove-all +junk -- to:glamira@eevie.ro
          '';
        };
      };
    };

    services = {
      mbsync = {
        enable = true;
        postExec = "${pkgs.notmuch}/bin/notmuch new";
        frequency = "*-*-* *:*:00";
        verbose = true;
      };
    };
  };
}
