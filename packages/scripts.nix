{
  pkgs,
  lib,
  ...
}: let
  my-lib = import ./../lib {inherit pkgs;};
  haskell-programs = import ./haskell.nix;
  make-haskell-program = name: let
    p = haskell-programs.programs."${name}";
    loadFromBins =
      lib.strings.concatStringsSep
      ","
      (lib.lists.forEach p.loadFromBins (bin: "\"${pkgs.${bin}}\""));
  in
    pkgs.writers.writeHaskellBin
    p.name
    {
      libraries = lib.lists.forEach p.libraries (lib: pkgs.haskellPackages."${lib}");
    }
    (lib.strings.replaceString
      "$(loadEnv SearchPath)"
      ("loadFromBins [ " + loadFromBins + " ]")
      (haskell-programs.haskellLanguagePragmas + "\n\n" + p.text));
in {
  get-active-window = pkgs.writeShellApplication {
    name = "get-active-window";
    runtimeInputs = [pkgs.socat pkgs.jq];
    text = ''
      #!/usr/bin/env bash

      hyprctl activewindow -j | jq '.class' | awk '{print tolower($0)}'

      socat -u UNIX-CONNECT:"$XDG_RUNTIME_DIR"/hypr/"$HYPRLAND_INSTANCE_SIGNATURE"/.socket2.sock - |
        stdbuf -o0 awk -F '>>|,' -f ${./../scripts/active-window-parse.awk}
    '';
  };

  tv-toggle = pkgs.writeShellApplication {
    name = "tv-toggle";
    runtimeInputs = [pkgs.jq];
    text = ''
      #!/usr/bin/env bash

      num_monitors=$(hyprctl monitors -j | jq length)

      if [ "$num_monitors" -lt "4" ]; then
        hyprctl keyword monitor "HDMI-A-1,1920x1080@60,0x395,1"
      else
        hyprctl keyword monitor "HDMI-A-1,disabled"
      fi
    '';
  };

  tv-status = my-lib.nuShellScript {
    name = "tv-status";
    runtimeInputs = [pkgs.jq];
    text = ''
      #!/usr/bin/env nu

      # This is hard-coded for thelxinoe
      let num_monitors = hyprctl monitors -j | jq length
      let alt = if $num_monitors == "4" {
                  "on"
                } else {
                  "off"
                }

      { alt: $alt
      } | to json -r
    '';
  };

  calendar-notify = make-haskell-program "calendar-notify";
  calendar-status = make-haskell-program "calendar-status";
  deploy = make-haskell-program "deploy";
  dotfiles = make-haskell-program "dotfiles";
  email-status = make-haskell-program "email-status";
  hyprshade-ctl = make-haskell-program "hyprshade-ctl";
  webcam-status = make-haskell-program "webcam-status";
}
