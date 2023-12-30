#!/usr/bin/env bash

entry=`eww get calEntry`
emacs -batch -l ~/.emacs.d/agenda.el -eval '(org-batch-agenda-csv "a")' 2>/dev/null | csvjson --no-header-row | jq "nth ($entry; .[] | select (.a != null))"
