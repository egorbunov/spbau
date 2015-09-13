#! /bin/bash
cal | cat -v | grep -o "\(_^H[0-9]\)\{1,2\}" | sed 's/_^H//g'

# -v key f
# sed command used here to replace all special highlight characters with zero-string