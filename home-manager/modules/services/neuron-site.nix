/****************************************************************************
  * neuron-site module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  sources = import ../../../nix/sources.nix;
  neuron = import sources.neuron;
  cfg = config.evie.services.neuron-site;
  updateScript = pkgs.writeScript "neuronSiteUpd.sh"
    ''
#!${pkgs.bash}/bin/bash

cd /home/evie/code/wiki
${pkgs.git}/bin/git pull
${neuron.default}/bin/neuron gen
cd .neuron/output
${pkgs.coreutils}/bin/cp -rL * /mnt/raid/wiki
    '';
in
{
  imports = [ ];

  options.evie.services.neuron-site = {
    enable = lib.options.mkEnableOption "Enable neuron-site";
  };

  config = lib.mkIf cfg.enable {
    systemd.user.services.neuronSiteUpdater = {
      Unit = {
        Description = "Neuron-Site Updater";
      };

      Service = {
        ExecStart = "${updateScript}";
      };
    };

    systemd.user.timers.neuronSiteUpdaterTimer = {
      Unit = {
        Description = "Neuron-Site Updater Timer";
      };

      Timer = {
        OnCalendar = "hourly";
        Unit = "neuronSiteUpdater.service";
      };

      Install = { WantedBy = [ "timers.target" ]; };
    };

  };
}