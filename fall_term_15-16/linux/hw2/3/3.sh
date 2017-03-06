#! /bin/bash

# u can specify directory for bfs
FLD=$1
if [[ -z "$FLD" ]]; then
	FLD="."
fi

files=$(find -- "$FLD" -mindepth 0 -maxdepth 0 -printf "%f\n" | sort)
IFS='\n'
let "i=0"
while [[ ! -z "$files" ]]; do
	let "i+=1"
	echo "$files"
	echo ""
	files=$(find -- "$FLD" -mindepth $i -maxdepth $i -printf "%f\n" | sort)
done
