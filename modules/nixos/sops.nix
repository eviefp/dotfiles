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
        garnix_netrc = { };
        yubiAuthFile = { };
      };
    };
  };
}
