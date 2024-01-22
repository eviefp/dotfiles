#!/usr/bin/env bash

ps aux --sort=-pcpu | head -n 10 | jq -sR '[sub("\n$";"") | splits("\n") | sub("^ +";"") | [splits(" +")]] | .[0] as $header | .[1:] | [.[] | [. as $x | range($header | length) | {"key": $header[.], "value": $x[.]}] | from_entries | .COMMAND |= sub(".*/"; "") ] | [to_entries[]]'

# | .[] | [.COMMAND |= sub(".*/"; "")]

