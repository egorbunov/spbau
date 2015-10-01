#/bin/bash

words=$(cat input.txt | tr " " "\n" | sort | uniq )
IFS=$'\n'
for w in $words; do
	sed -i "s/\($w\)/\*\1\*/g" input.txt
	sed -i "s/\*\($w\)\*/\1/1" input.txt
	echo $w
done
