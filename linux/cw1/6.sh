#/bin/bash

words=$(cat $1 | tr "[:space:][:punct:]" "\n" | sort | uniq )
IFS=$'\n'
for w in $words; do
	sed -i "s/\($w\)/\*\1\*/g" $1
	sed -i "s/\*\($w\)\*/\1/1" $1
done
