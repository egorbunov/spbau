#/bin/bash

if [ -z $1 ]
then
	echo "Specify file with citations!"
	exit 1
fi

max=$(sed '2!d' $1)
rnd_index=$(( RANDOM % max ))
from_line=$(cat $1 | grep -n "\* $rnd_index" | sed 's/:\* .\+//g')
let "from_line+=1"

let "to_index=rnd_index+1"
to_line=$(cat $1 | grep -n "\* $to_index" | sed 's/:\* .\+//g')

if [ -z $to_line ] 
then
	to_line="\$"
else
	let "to_line-=1"
fi

sed -n "$from_line","$to_line"p "$1"

