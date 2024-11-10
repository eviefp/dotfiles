{ dotfiles, pkgs, lib, osConfig, ... }:
{
  config = lib.mkIf (lib.elem "hyprland" osConfig.evie.wayland.compositors) {
    home = {
      file.".config/hypr/hyprlock.conf".text = ''
        general {
          pam_module = hyprlock
        }

        background {
            monitor =
            path = /home/evie/.config/wallpaper/rev.jpg
            color = rgba(25, 20, 20, 1.0)

            # all these options are taken from hyprland, see https://wiki.hyprland.org/Configuring/Variables/#blur for explanations
            blur_passes = 0 # 0 disables blurring
            blur_size = 7
            noise = 0.0117
            contrast = 0.8916
            brightness = 0.8172
            vibrancy = 0.1696
            vibrancy_darkness = 0.0
        }

        input-field {
            monitor =
            size = 200, 50
            outline_thickness = 3
            dots_size = 0.33 # Scale of input-field height, 0.2 - 0.8
            dots_spacing = 0.15 # Scale of dots' absolute size, 0.0 - 1.0
            dots_center = false
            outer_color = rgb(151515)
            inner_color = rgb(200, 200, 200)
            font_color = rgb(10, 10, 10)
            fade_on_empty = true
            placeholder_text = <i>password</i> # Text rendered in the input box when it's empty.
            hide_input = false

            position = 0, -20
            halign = center
            valign = center
        }

        label {
            monitor =
            text = Hi $USER ☭
            color = rgba(200, 200, 200, 1.0)
            font_size = 25
            font_family = Noto Sans

            position = 0, 80
            halign = center
            valign = center
        }
      '';

      packages = [
        dotfiles.hyprlock.packages.${pkgs.system}.hyprlock
      ];
    };
  };
}
