#! /bin/bash

# Repeats given string; takes 2 args: 
# 	1. str to repeat
#   2. number - number of repeats
function repeatStr {
	if [[ $2 -gt 0 ]]; then
		printf -- "$1%.0s" $(seq 1 $2)
	fi
}

# one arg: filename
function getDepth {
	cnt=$(echo -- "$1" | grep '/' -o | wc -l)
	l=$(echo -- "$1" | grep '/$' -o | wc -l)
	echo $((cnt-l))
}

# two args: filename and depth
function printFile {
	if [[ "$depth" -gt "0" ]]; then
		printf "|"
		let "rep=$depth * 2 - 1"
		repeatStr "-" $rep
	fi
	printf -- "%s\n" "$1"
}

# u can specify directory for bfs
FLD=$1
if [[ -z "$FLD" ]]; then
	FLD="."
fi
baseDepth=$(getDepth "$FLD")
find "$FLD" | while read f;
do
	depth=$(getDepth "$f")
	let "depth-=baseDepth"
	filename="${f##*/}"
	if [[ "$depth" == "0" ]]; then
		filename=$f
	fi
	printFile "$filename" "$depth"
done
