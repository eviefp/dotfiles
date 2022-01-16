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
    neomutt.enable = true;
  };
in
  { inherit eevie;
  }
