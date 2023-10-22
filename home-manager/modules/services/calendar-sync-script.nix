/****************************************************************************
  * Calendar Sync Script
  **************************************************************************/
{ resholve, pkgs, url, org, ... }:
resholve.writeScriptBin "calendar-sync-script"
{
  inputs = [
    pkgs.coreutils
    pkgs.wget
    pkgs.ical2orgpy
  ];
  interpreter = "${pkgs.bash}/bin/bash";
  execer = [
    "cannot:${pkgs.wget}/bin/wget"
    "cannot:${pkgs.ical2orgpy}/bin/ical2orgpy"
  ];
} ''
  TMP_FILE=$(mktemp)

  wget -O $TMP_FILE '${url}'

  ical2orgpy $TMP_FILE '${org}'
''
