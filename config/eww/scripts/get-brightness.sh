#!/usr/bin/env bash

host=`hostname`

if [[ "$host" == "thelxinoe" ]]; then
    echo "100"
else
    light
fi
