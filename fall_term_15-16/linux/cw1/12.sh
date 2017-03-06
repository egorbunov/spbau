#!/bin/bash

chr() {
  [ "$1" -lt 256 ] || return 1
  printf "\\$(printf '%03o' "$1")"
}

ord() {
  LC_CTYPE=C 
}

rm -f $2
cat $1 | while IFS= read -r -N 1 c;
do
	let "next_code=$(printf '%d' "'$c")+1"
	printf "\\$(printf '%03o' "$next_code")" >> $2
done