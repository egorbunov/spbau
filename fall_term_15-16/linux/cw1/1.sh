#!/bin/bash

# color is grayscale if R = G = B, so...
echo "#060606" | grep -P '(?i)#([\da-f]{2})\1\1'
