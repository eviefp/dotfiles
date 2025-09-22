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
            mods =
              pkgs.linkFarmFromDrvs "mods"
              (
                builtins.attrValues {
                  Fabric-API = pkgs.fetchurl {
                    url = "https://cdn.modrinth.com/data/P7dR8mSH/versions/UapVHwiP/fabric-api-0.92.6%2B1.20.1.jar";
                    sha512 = "2bd2ed0cee22305b7ff49597c103a57c8fbe5f64be54a906796d48b589862626c951ff4cbf5cb1ed764a4d6479d69c3077594e693b7a291240eeea2bb3132b0c";
                  };
                  Create-Fabric = pkgs.fetchurl {
                    url = "https://cdn.modrinth.com/data/Xbc0uyRg/versions/7Ub71nPb/create-fabric-0.5.1-j-build.1631%2Bmc1.20.1.jar";
                    sha512 = "73ff936492c857ae411c10cae0194d64a56b98a1a7a9478ca13fe2a6e3ee155e327cf4590a3888aaa671561b4cf74de97f2f44224d7981b03a546e36236c3de2";
                  };
                }
              );
          };
        };
      };
    };
  };
}
