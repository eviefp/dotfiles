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
          userName = "evieciobanu";
          realName = "Evie Ciobanu";
          signature = {
            text = ''

              -- Evie Ciobanu
            '';
            showSignature = "append";
          };
          passwordCommand =
            "${pkgs.coreutils}/bin/cat /home/evie/.secrets/hydro.pwd";
          smtp = { host = "127.0.0.1"; };
          imap = { host = "127.0.0.1"; };
          mbsync = {
            enable = true;
            create = "both";
            expunge = "both";
            extraConfig.account = {
              Port = 1143;
              SSLType = "None";
            };
          };
          msmtp = {
            enable = true;
            extraConfig = {
              port = "1025";
              tls = "off";
              tls_trust_file = "";
              tls_certcheck = "off";
            };
          };
          notmuch.enable = true;
          flavor = "plain";
        };
        alexaeviest = {
          primary = false;
          address = "alexa.eviest@gmail.com";
          userName = "alexa.eviest@gmail.com";
          realName = "Evie Ciobanu";
          signature = {
            text = ''

              -- Evie Ciobanu
            '';
            showSignature = "append";
          };
          passwordCommand =
            "${pkgs.coreutils}/bin/cat /home/evie/.secrets/alexaeviest.pwd";
          notmuch.enable = true;
          flavor = "gmail.com";
          msmtp.enable = true;
          mbsync = {
            enable = true;
            create = "both";
            expunge = "both";
            remove = "none";
            patterns = [
              "INBOX"
              "\\[Gmail\\]/All Mail"
              "\\[Gmail\\]/Drafts"
              "\\[Gmail\\]/Spam"
              "\\[Gmail\\]/Sent Mail"
            ];
          };
          smtp = {
            host = "smtp.gmail.com";
            tls.useStartTls = true;
            port = 587;
          };
          imap.host = "imap.gmail.com";
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
      };
    };

    services = {
      mbsync = {
        enable = true;
        postExec = "${pkgs.notmuch}/bin/notmuch new";
      };
    };
  };
}
