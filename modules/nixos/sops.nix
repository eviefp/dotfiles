{ dotfiles, ... }:
{
  imports = [
    dotfiles.sops-nix.nixosModules.sops
  ];

  sops = {
    defaultSopsFile = ../../secrets/secrets/secrets.yaml;
    age.sshKeyPaths = [ "/home/evie/.ssh/id_ed25519" ];
    secrets = {
      garnix_netrc = { };
      yubiAuthFile = { };
    };
  };
}
