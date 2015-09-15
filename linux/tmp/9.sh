#! /bin/bash

files=$(find . -type f ! -name "*.sh" -print)

for f in $files; do
	rename -v 's/([^\/]*)$/_$1/' $f
done

