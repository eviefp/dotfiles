{ dotfiles, ... }:
{
  imports = [
    dotfiles.sops-nix.homeManagerModules.sops
    dotfiles.self.homeManagerModules.ect
  ];

  sops = {
    defaultSopsFile = ../../secrets/secrets/secrets.yaml;
    age.sshKeyPaths = [ "/home/evie/.ssh/id_ed25519" ];
    secrets = {
      evie_password = { };
      evie_certificate = { };
      garnix_password = { };
      gmail_password = { };
      ect_yaml = {
        sopsFile = ../../secrets/secrets/ect.yaml;
      };
    };
  };

}
