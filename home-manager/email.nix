{ pkgs, asServer ? true }:
let
  gmail = {
    primary = true;
    address = "vlad@cvlad.info";
    flavor = "gmail.com";
    msmtp.enable = true;
    mbsync =
      if asServer
        then
            {
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
            }
      else { enable = false; };
    notmuch.enable = true;
    smtp = {
      host = "smtp.gmail.com";
      tls.useStartTls = true;
      port = 587;
    };
    imap.host =
      if asServer
        then "imap.gmail.com"
        else null;
    realName = "Vladimir Ciobanu";
    userName = "admin@cvlad.info";
    passwordCommand = "${pkgs.coreutils}/bin/cat /home/vlad/.secrets/cvlad.pwd";
    neomutt = {
      enable = true;
      sendMailCommand = "msmtpq --read-envelope-from --read-recipients";
    };
  };
in
  { inherit gmail;
  }
