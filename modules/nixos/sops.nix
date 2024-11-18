{ dotfiles, ... }:
{
  imports = [
    dotfiles.sops-nix.nixosModules.sops
  ];

  sops = {
    defaultSopsFile = ../../secrets/secrets/secrets.yaml;
    age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
    secrets = {
      garnix_netrc = { };
      yubiAuthFile = { };
    };
  };
}
