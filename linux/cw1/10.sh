#! /bin/bash
cal -N $2 $3 | grep "\s$1\s" | grep -Po "[a-zA-Z]{2}"
