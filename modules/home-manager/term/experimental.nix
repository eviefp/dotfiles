/**
**************************************************************************
* experimental terminal apps
*
*************************************************************************
*/
{
  dotfiles,
  lib,
  config,
  pkgs,
  ...
}: let
  cfg = config.evie.term.experimental;
  mapMain = exp: ''
    print "${exp.name} - ${exp.short}"
  '';
  mapDetails = exp: ''
    def "main ${exp.name}" [] {
      print '${exp.name} - ${exp.short}'
      print '${exp.help}'
    }
  '';
  exp = dotfiles.self.lib.nuShellScript {
    name = "exp";
    text = ''
      def main [] {
        ${lib.strings.concatStringsSep "\n" (lib.lists.map mapMain cfg.experiments)}
      }

      ${lib.strings.concatStringsSep "\n" (lib.lists.map mapDetails cfg.experiments)}
    '';
  };
in {
  options.evie.term.experimental = {
    enable = lib.mkEnableOption "term defaults";
    experiments = lib.mkOption {
      type = lib.types.listOf (lib.types.submodule {
        options = {
          package = lib.mkOption {
            type = lib.types.package;
            default = dotfiles.self.packages.${pkgs.system}.peroxide;
          };
          name = lib.mkOption {
            type = lib.types.str;
          };
          short = lib.mkOption {
            type = lib.types.str;
          };
          help = lib.mkOption {
            type = lib.types.str;
          };
        };
      });

      default = [
        {
          name = "ncdu";
          package = pkgs.ncdu;
          short = "disk usage and cleanup TUI";
          help = ''
            ncdu -- opens the TUI from CWD
          '';
        }
        {
          name = "choose";
          package = pkgs.choose;
          short = "ws-based text filtering";
          help = ''
            print "1 2 3" | choose 1 => 2
          '';
        }
        {
          name = "gping";
          package = pkgs.gping;
          short = "ping with history graph";
          help = ''
            gping
          '';
        }
        {
          name = "ffmpegthumbnailer";
          package = pkgs.ffmpegthumbnailer;
          short = "thumbnail from video";
          help = ''
            ffmpegthumbnailer -i<file> -o<file> -t<%/hh:mm:ss>
          '';
        }
        {
          name = "bandwhich";
          package = pkgs.bandwhich;
          short = "show bandwidth usage per process";
          help = ''
            sudo bandwhich
          '';
        }
        {
          name = "confetty";
          package = pkgs.confetty;
          short = "yay";
          help = ''
            confetty
          '';
        }
        {
          name = "serie";
          package = pkgs.serie;
          short = "git graph";
          help = ''
            serie
          '';
        }
        {
          name = "rainfrog";
          package = pkgs.rainfrog;
          short = "sql tui";
          help = ''
            rainfrog --url postgres://username:password@localhost:port/dbname
          '';
        }
        {
          # TODO: only for wifi
          name = "impala";
          package = pkgs.impala;
          short = "wifi tui";
          help = ''
            impala
          '';
        }
        {
          # TODO: only for bluetooth?
          name = "bluetui";
          package = pkgs.bluetui;
          short = "bluetooth tui";
          help = ''
            bluetui
          '';
        }
        {
          name = "serpl";
          package = pkgs.serpl;
          short = "search and replace tui";
          help = ''
            serpl
          '';
        }
        {
          name = "isw";
          package = dotfiles.self.packages.${pkgs.system}.isw;
          short = "stopwatch tui";
          help = ''
            isw -i seconds[,seconds,...]
          '';
        }
      ];
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = (lib.lists.map (exp: exp.package) cfg.experiments) ++ [exp];
  };
}
