{ dotfiles, ... }:
{
  imports = [
    dotfiles.sops-nix.homeManagerModules.sops
  ];

  sops = {
    defaultSopsFile = ../../secrets/secrets/secrets.yaml;
    age.sshKeyPaths = [ "/home/evie/.ssh/id_ed25519" ];
    secrets = {
      evie_password = { };
      evie_certificate = { };
      garnix_password = { };
      gmail_password = { };
      gmailCalendarClientId = { };
      gmailCalendarClientSecret = { };
    };
  };

}
