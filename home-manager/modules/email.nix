/****************************************************************************
  * Email module
  *
  * Set up email account, imap sync, notmuch mail management.
  **************************************************************************/
{ lib, config, pkgs, ... }:
let cfg = config.evie.email;
in
{
  imports = [ ];

  options.evie.email = {
    enable = lib.options.mkEnableOption "Enable emails.";
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

          signature = {
            text = ''

              -- Evie
            '';
            showSignature = "append";
          };

          passwordCommand = "${pkgs.coreutils}/bin/cat /home/evie/.secrets/px-key";

          imap = {
            host = "nixos";
            port = 1143;
            tls = {
              enable = true;
              useStartTls = true;
              certificatesFile = ./peroxide.pem;
            };
          };

          smtp = {
            host = "nixos";
            port = 1025;
            tls = {
              enable = true;
              useStartTls = true;
              certificatesFile = ./peroxide.pem;
            };
          };

          mbsync = {
            enable = true;
            create = "both";
            remove = "both";
          };

          msmtp = {
            enable = true;
            tls.fingerprint = "20:25:0E:A6:C0:3E:E1:3B:AE:9A:B1:12:B5:A2:C3:04:89:02:A4:F5:38:74:37:EC:D5:23:F0:82:6F:77:3F:C7";
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

          passwordCommand = "${pkgs.coreutils}/bin/cat /home/evie/.secrets/ae";

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

        gmail-old = {
          primary = false;
          address = "admin@cvlad.info";
          userName = "admin@cvlad.info";
          realName = "Evie Ciobanu";

          signature = {
            text = ''

              -- Evie
            '';
            showSignature = "append";
          };

          passwordCommand = "${pkgs.coreutils}/bin/cat /home/evie/.secrets/cv";

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
        new.tags = [ "unread" "inbox" ];
        hooks = {
          postNew = ''
            notmuch tag +evie -- to:*@eevie.ro
            notmuch tag +gmail -- to:*evie*@gmail.com
            notmuch tag +del -- to:*@cvlad.info
            notmuch tag +hf -- from:*@haskell.foundation
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
