{ dotfiles, lib, config, ... }:
let
  cfg = config.evie.sops;
in
{
  options.evie.sops = {
    enable = lib.mkEnableOption "sops defaults";
  };

  imports = [
    dotfiles.sops-nix.nixosModules.sops
  ];

  config = lib.mkIf cfg.enable {
    sops = {
      defaultSopsFile = ../../secrets/secrets/secrets.yaml;
      age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
      secrets = {
        garnix_netrc = {
          mode = "444";
        };
        yubiAuthFile = {
          mode = "444";
        };

        evie_password = {
          mode = "400";
          owner = "evie";
        };

        evie_certificate = {
          mode = "444";
        };

        garnix_password = {
          mode = "400";
          owner = "evie";
        };

        gmail_password = {
          mode = "400";
          owner = "evie";
        };

        gmailCalendarClientId = {
          mode = "400";
          owner = "evie";
        };

        gmailCalendarClientSecret = {
          mode = "400";
          owner = "evie";
        };

        spotifyAppClientId = {
          mode = "400";
          owner = "evie";
        };
        spotifyAppClientSecret = {
          mode = "400";
          owner = "evie";
        };

        weatherKey = {
          mode = "400";
          owner = "evie";
        };
      };
    };
  };
}
