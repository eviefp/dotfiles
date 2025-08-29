{ lib, config, pkgs, ... }:
let
  cfg = config.evie.system.gpg;
in
{
  options.evie.system.gpg = {
    enable = lib.mkEnableOption "calendar defaults";
  };

  config = lib.mkIf cfg.enable {
    programs.gpg = {
      enable = true;

      publicKeys = [
        # {
        #   source = ../../../config/272D83521C488CCD-2024-11-09.asc;
        #   trust = "ultimate";
        # }
        {
          source = ../../../config/9B61460007838C21-2025-08-28.asc;
          trust = "ultimate";
        }
      ];

      # https://support.yubico.com/hc/en-us/articles/4819584884124-Resolving-GPG-s-CCID-conflicts
      scdaemonSettings = {
        disable-ccid = true;
      };

      settings = {
        personal-cipher-preferences = "AES256 AES192 AES";
        personal-digest-preferences = "SHA512 SHA384 SHA256";
        personal-compress-preferences = "ZLIB BZIP2 ZIP Uncompressed";
        default-preference-list = "SHA512 SHA384 SHA256 AES256 AES192 AES ZLIB BZIP2 ZIP Uncompressed";
        cert-digest-algo = "SHA512";
        s2k-digest-algo = "SHA512";
        s2k-cipher-algo = "AES256";
        charset = "utf-8";
        fixed-list-mode = true;
        no-comments = true;
        no-emit-version = true;
        no-greeting = true;
        keyid-format = "0xlong";
        list-options = "show-uid-validity";
        verify-options = "show-uid-validity";
        with-fingerprint = true;
        require-cross-certification = true;
        no-symkey-cache = true;
        use-agent = true;
        throw-keyids = true;
        armor = true;
      };
    };

    services.gpg-agent = {
      enable = true;
      enableNushellIntegration = true;
      enableScDaemon = true;
      enableSshSupport = true;

      # https://github.com/drduh/config/blob/master/gpg-agent.conf
      defaultCacheTtl = 60;
      defaultCacheTtlSsh = 60;
      maxCacheTtl = 120;
      maxCacheTtlSsh = 120;
      pinentry.package = pkgs.pinentry-gtk2;
      extraConfig = ''
        ttyname $GPG_TTY
        no-allow-external-cache
      '';
    };
  };
}
