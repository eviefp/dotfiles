{ dotfiles, ... }:
{
  imports = with dotfiles.self.nixosModules; [
    users
    nix-settings
    openvpn-server

    dotfiles.disko.nixosModules.disko
    dotfiles.sops-nix.nixosModules.sops
    dotfiles.home-manager.nixosModules.home-manager
    {
      home-manager = {
        backupFileExtension = "backup";
        useGlobalPkgs = true;
        useUserPackages = true;
        extraSpecialArgs = { inherit dotfiles; };
        users.evie = ./home.nix;
      };
    }

    ./hardware.nix
    ./disko.nix
  ];

  config = {
    evie = {
      users.enable = true;
      nix-settings.enable = true;
      openvpn-server.enable = true;
    };

    networking = {
      hostName = "jellyfin";
      firewall.allowedTCPPorts = [ 80 443 ];
      useDHCP = true;
    };

    services.openssh.enable = true;

    sops = {
      defaultSopsFile = ../../secrets/secrets/secrets.yaml;
      age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
      secrets = {
        thelxinoe-openvpn-key = {
          mode = "444";
        };
      };
    };


    services.nginx = {
      enable = true;
      recommendedProxySettings = true;
      recommendedTlsSettings = true;
      clientMaxBodySize = "20M";
      virtualHosts."jellyfin.evie.ro" = {
        forceSSL = true;
        enableACME = true;
        locations."/" = {
          proxyPass = "http://10.8.0.2:8096";
          extraConfig = ''
            proxy_buffering off;
            proxy_set_header X-Forwarded-Protocol $scheme;
          '';
        };
        locations."/socket" = {
          proxyPass = "http://10.8.0.2:8096";
          proxyWebsockets = true; # needed if you need to use WebSocket
          extraConfig = ''
            proxy_set_header X-Forwarded-Protocol $scheme;
          '';
        };
      };
    };
    security.acme = {
      acceptTerms = true;
      defaults.email = "acme@evie.ro";
    };
  };
}
