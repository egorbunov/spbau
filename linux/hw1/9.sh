#! /bin/bash

files=$(find . -type f ! -name "*.sh" -mtime +0 -mtime -3 -print)

IFS=$'\n'
for f in $files; do
	rename -v 's/([^\/]*)$/_$1/' $f
done

# better:
find . -type f -mtime -2 -mtime 1 ! -name "*.sh" | while read f
do
	name="`basename "$f"`"
	dir="`dirname "$f"`"
	# rename -- 
done
