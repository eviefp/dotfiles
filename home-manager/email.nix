{ pkgs }:
let
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
    passwordCommand = "${pkgs.coreutils}/bin/cat /home/evie/.secrets/hydro.pwd";
    smtp = {
      host = "fractal";
    };
    imap = {
      host = "fractal";
    };
    mbsync = {
      enable = true;
      create = "both";
      expunge = "both";
      extraConfig.account = {
        Port = 1143;
        SSLType = "None";
      };
    };
    msmtp.enable = true;
    notmuch.enable = true;
    neomutt.enable = true;
  };
in
  { inherit eevie;
  }
