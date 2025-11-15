{
  config,
  lib,
  pkgs,
  dotfiles,
  ...
}: let
  cfg = config.evie.minecraft;
  mode = {
    survival = 0;
    creative = 1;
    adventure = 2;
    default = 5;
  };
  difficulty = {
    peaceful = 0;
    easy = 1;
    normal = 2;
    hard = 3;
  };
  modHashes = lib.importJSON ./minecraft/mods.json;
in {
  imports = [
    dotfiles.nix-minecraft.nixosModules.minecraft-servers
  ];

  options.evie.minecraft = {
    enable = lib.options.mkEnableOption "Enable minecraft server.";
  };

  config = lib.mkIf cfg.enable {
    services.minecraft-servers = {
      enable = true;
      eula = true;
      openFirewall = true;
      # using this because I don't want to stop the one above
      # I should migrate it to here and then this should be fine
      user = "minecraft-server";

      servers = {
        first-mara = {
          enable = true;
          autoStart = true;
          package = pkgs.fabricServers.fabric-1_21_8; # .override { loaderVersion = ""; };

          operators = {
            evie__ro = "372ca84a-dda2-4060-9f54-9b08e5dc9e6c";
            gia1gia2gia3 = "54d21c56-1ea3-4a7b-8970-878b46e59424";
          };

          serverProperties = {
            gamemode = mode.creative;
            difficulty = difficulty.peaceful;
            motd = "evie - mara - gia";
            server-port = 25565;
            allow-flight = true;
            force-gamemode = true;
            pvp = false;
            spawn-monsters = false;
            # white-list = true;
          };
        };

        modded-1 = {
          enable = true;
          autoStart = true;
          package = pkgs.fabricServers.fabric-1_20_1; # .override { loaderVersion = ""; };

          serverProperties = {
            gamemode = mode.creative;
            difficulty = difficulty.peaceful;
            motd = "evie - modded";
            server-port = 25566;
            allow-cheats = true;
            allow-flight = true;
            force-gamemode = true;
            pvp = false;
            spawn-monsters = false;

            # white-list = true;
          };

          symlinks = {
            mods = pkgs.linkFarmFromDrvs "mods" (map (mod:
              pkgs.fetchurl {
                url = mod.url;
                sha512 = mod.sha512;
              })
            modHashes);
          };
        };
      };
    };
  };
}
