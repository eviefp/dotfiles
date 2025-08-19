#!/usr/bin/env nu

/home/evie/.config/waybar/scripts/get-next-calendar-entry.nu single
  | jq '.startDT + " " + .title'
  | sd "^\"\(../..\)/.... \(..:..\) \(.*\)\"$" '$1@$2 $3'
