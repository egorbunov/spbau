#!/bin/bash
while true; do sleep 5s ; ps -aux --sort=-%cpu | tail -n +2 | head -n 1 | echo; done &