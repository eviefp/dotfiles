#!/usr/bin/env bash

entry=`eww get calEntry`

emacs \
      -batch \
      -l ~/.emacs.d/agenda.el \
      -eval '(org-batch-agenda-csv "a")' \
      2>/dev/null \
	  | csvjson --no-header-row \
	  | jq 'nth('$entry'; .[] | select(.a != null) | select(.f > (now | strftime("%Y-%m-%d")) or (.g[0:5] > (now | strftime("%H:%M")))))'
#                        array ^    ^^ nonempty            either tomorrow+ ^^^^                        ^^^^ or later than now

# Sample output of batch & csvjson:
#[
#  {
#    "a": null,
#    "b": "----------------",
#    "c": null,
#    "d": null,
#    "e": null,
#    "f": null,
#    "g": "8:00......",
#    "h": null,
#    "i": null,
#    "j": null,
#    "k": "2023-12-30"
#  },
#  {
#    "a": null,
#    "b": "----------------",
#    "c": null,
#    "d": null,
#    "e": null,
#    "f": null,
#    "g": "10:00......",
#    "h": null,
#    "i": null,
#    "j": null,
#    "k": "2023-12-30"
#  },
#  {
#    "a": "cal-name",
#    "b": "event name",
#    "c": "block",
#    "d": null,
#    "e": null,
#    "f": "2023-12-30",
#    "g": "12:00-13:00",
#    "h": null,
#    "i": null,
#    "j": 1000.0,
#    "k": "2023-12-30"
#  },
#]
