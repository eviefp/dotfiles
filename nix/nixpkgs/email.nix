## Under construction

  accounts.email.accounts.cvlad = {
    primary = true;
    address = "admin@cvlad.info";
    realName = "Vladimir Ciobanu";
    flavor = "gmail.com";
    passwordCommand = getPassword "Email/cvlad";
    maildir.path = "cvlad";
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
      ];
    };
    notmuch.enable = true;
    # neomutt = {
    #   enable = true;
    #   sendMailCommand = "msmtpq --read-envelope-from --read-recipients";
    # };
  };

  programs.mbsync.enable = true;
  programs.msmtp.enable = true;
  programs.notmuch.enable = true;
  # programs.neomutt = {
  #   enable = true;
  #   vimKeys = true;
  # };

  services.mbsync.enable = true;
