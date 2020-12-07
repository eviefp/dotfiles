{ pkgs, ... }:
let
  pass = "${pkgs.pass}/bin/pass";
  gmail = {
    primary = true;
    address = "vlad@cvlad.info";
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
    notmuch.enable = true;
    smtp = {
      host = "smtp.gmail.com";
      tls.useStartTls = true;
      port = 587;
    };
    imap.host = "imap.gmail.com";
    realName = "Vladimir Ciobanu";
    userName = "admin@cvlad.info";
    passwordCommand = "${pass} Email/cvlad-app";
    neomutt = {
      enable = true;
      sendMailCommand = "msmtpq --read-envelope-from --read-recipients";
    };
  };
in
  { inherit gmail;
  }
