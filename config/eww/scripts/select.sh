#!/usr/bin/env bash

proc=`eww get proc`
idx=`eww get procEntry`
pid=`echo $proc | jq --raw-output ".[] | select(.key == $idx) | .value.PID"`
kill -9 $pid
