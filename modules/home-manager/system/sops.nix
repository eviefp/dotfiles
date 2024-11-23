{ dotfiles, ... }:
{
  imports = [
    dotfiles.sops-nix.homeManagerModules.sops
  ];

  sops = {
    defaultSopsFile = ../../secrets/secrets/secrets.yaml;
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

}
