{ dotfiles, lib, config, ... }:
let
  cfg = config.evie.system.sops;
in
{
  options.evie.system.sops = {
    enable = lib.mkEnableOption "sops defaults";
  };

  imports = [
    dotfiles.sops-nix.homeManagerModules.sops
  ];

  config = lib.mkIf cfg.enable {
    sops = {
      defaultSopsFile = ../../../secrets/secrets/secrets.yaml;
      age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
      secrets = {
        evie_password = { };
        evie_certificate = { };
        garnix_password = { };
        gmail_password = { };
        gmailCalendarClientId = { };
        gmailCalendarClientSecret = { };
        spotifyAppClientId = { };
        spotifyAppClientSecret = { };
      };
    };
  };

}
