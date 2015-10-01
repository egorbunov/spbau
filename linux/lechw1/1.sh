#!/bin/bash
# (?i) -- case-insensitive modifier
# my 33 symbols VS 37 symbols in regex from slides

echo "23:b9:7E:60:39:E9" | grep -P '(?i)^([\da-f]{2}:){5}[\da-f]{2}$'

