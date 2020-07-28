#!/bin/bash

gpu=$(nvidia-smi | awk '$14=="Default" {print $13}')

echo $gpu

exit 0
