#! /bin/bash
cnt=$(find . -type f -printf "%u\n" | grep $USER | wc -l)
echo "File number = $cnt"