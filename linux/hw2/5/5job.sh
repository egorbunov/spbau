#! /bin/bash

function isOnline {
	ans=$(curl --silent "https://api.vk.com/method/users.get?uid=1820751&fields=online" | grep -Po "\"online\":\d")
	if [ "$ans" == "\"online\":1" ]; then
		echo 1
	else
		echo 0
	fi 
}
ofile="/tmp/.iskuzonline.txt"
prev_status=""
if [ ! -f "$ofile" ]; then
	echo "NOT EXIST"
	prev_status=$(isOnline)
    echo "$prev_status" > "$ofile"
else
	prev_status=$(cat "$ofile")
fi

status=$(isOnline)

if [ "$status" != "$prev_status" ]; then
	if [ "$status" == "1" ]; then
		echo "Anton is online now!" | wall
	else
		echo "Anton is offline now!" | wall
	fi
	echo "$status" > "$ofile"
fi


