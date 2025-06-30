{ config, lib, pkgs, ... }:
let
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
in
{
  imports = [ ];

  options.evie.minecraft = {
    enable = lib.options.mkEnableOption "Enable minecraft server.";
  };

  config = lib.mkIf cfg.enable {
    services.minecraft-server = {
      enable = true;
      eula = true;
      openFirewall = true;
      declarative = true;

      serverProperties = {
        gamemode = mode.survival;
        difficulty = difficulty.peaceful;
        motd = "evie";

        # white-list = true;
      };

      # whitelist = {
      #   "username" = "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx";
      # };

      package = pkgs.minecraft-server;

      # see hotspot docs for more info:
      # https://docs.oracle.com/en/java/javase/11/gctuning/garbage-first-garbage-collector-tuning.html
      # jvmOpts = builtins.concatStringsSep " " [
      #   "-Xms2G"
      #   "-Xmx3G"
      #   "-XX:+CMSIncrementalPacing"
      #   "-XX:+CMSClassUnloadingEnabled"
      #   "-XX:+ParallelRefProcEnabled"
      #   "-XX:+DisableExplicitGC"
      # ];
    };
  };
}
