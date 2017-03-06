#! /bin/bash
cnt=$(find . -type f -printf "%u\n" | grep $USER | wc -l)
echo "File number = $cnt"

# better solution: 
find . -user "$USER" -type f | wc -l