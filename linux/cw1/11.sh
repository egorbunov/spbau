#!/bin/sh
find /var -maxdepth 1  -type l -printf "%f\n"