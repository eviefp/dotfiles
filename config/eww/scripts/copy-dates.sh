#!/usr/bin/env bash

if [[ $1 -lt 10 ]]; then day="0"$1; else day=$1; fi
if [[ $2 -lt 10 ]]; then month="0"$2; else month=$2; fi
year=$3

cp=`date --date="$month/$day/$year" '+%Y-%m-%d %a 00:00'`

wl-copy $cp
