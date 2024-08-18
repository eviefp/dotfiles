/****************************************************************************
  * Email module
  *
  * Set up email account, imap sync, notmuch mail management.
  **************************************************************************/
{ dotfiles, config, pkgs, ... }:
{
  imports = [
    dotfiles.self.homeManagerModules.bower
  ];

  config = {
    accounts.email = {

      maildirBasePath = "/home/evie/mail";

      accounts = {
        eevie = {
          primary = true;
          address = "me@eevie.ro";
          userName = "evieciobanu..thelxinoe";
          realName = "Evie Ciobanu";

          signature = {
            text = ''

              -- Evie
            '';
            showSignature = "append";
          };

          passwordCommand = "${pkgs.coreutils}/bin/cat ${config.sops.secrets.evie_password.path}";

          imap = {
            host = "thelxinoe";
            port = 1143;
            tls = {
              enable = true;
              useStartTls = true;
              certificatesFile = "${config.sops.secrets.evie_certificate.path}";
            };
          };

          smtp = {
            host = "thelxinoe";
            port = 1025;
            tls = {
              enable = true;
              useStartTls = true;
              certificatesFile = "${config.sops.secrets.evie_certificate.path}";
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

        garnix = {
          primary = false;
          address = "eciobanu@garnix.io";
          userName = "eciobanu@garnix.io";
          realName = "Evie Ciobanu";

          signature = {
            text = ''

              -- Evie
            '';
            showSignature = "append";
          };

          passwordCommand = "${pkgs.coreutils}/bin/cat ${config.sops.secrets.garnix_password.path}";

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

        gmail-primary = {
          primary = false;
          address = "alexa.eviest@gmail.com";
          userName = "alexa.eviest@gmail.com";
          realName = "Evie Ciobanu";

          signature = {
            text = ''

              -- Evie
            '';
            showSignature = "append";
          };

          passwordCommand = "${pkgs.coreutils}/bin/cat ${config.sops.secrets.gmail_password.path}";

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
      alot = {
        enable = true;
      };
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
            notmuch tag +garnix -unsorted -- to:*@garnix.io
            notmuch tag +gmail -unsorted -- to:*evie*@gmail.com
            notmuch tag +hf -unsorted -- from:*@haskell.foundation
            notmuch tag +hf -unsorted -- to:*@haskell.foundation
            notmuch tag +sent -unsorted -- from:*@eevie.ro
            notmuch tag +sent -unsorted -- from:eciobanu@garnix.io
            notmuch tag --remove-all +junk -- to:glamira@eevie.ro
          '';
        };
      };
    };

    services = {
      mbsync = {
        enable = true;
        postExec = "${pkgs.notmuch}/bin/notmuch new";
        verbose = true;
      };
    };
  };
}
