#! /bin/bash

files=$(find . -type f ! -name "*.sh" -mtime +0 -mtime -3 -print)

for f in $files; do
	rename -v 's/([^\/]*)$/_$1/' $f
done
