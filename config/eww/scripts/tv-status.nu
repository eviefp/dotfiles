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
