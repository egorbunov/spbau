#! /bin/bash

function is_user_online {
	online_status_node=$(wget -q -O - http://vk.com/$1 | grep -o "<[^<]*profile_online_lv[^>]*>")
	if [[ $online_status_node == *"style=\"display: none;\""* ]]
	then
		echo false
	else
		echo true
	fi
}

function get_user_name {
	name=$(wget -q -O - http://vk.com/$1 \
		| grep "<[^<]*page_name fl_l ta_l[^>]*>" \
		| sed 's/[ ]*<[^<>]*>//g' \
		| iconv -f cp1251 -t UTF-8)
	echo $name
}

vk_user=id97429142 # put here vk user id (with id prefix in case of numeric value)
sleep_t=5s

username=$(get_user_name $vk_user)
username=$(echo $username | uniconv -encode Russian-Translit)
status=$(is_user_online $vk_user)

if [ $status == false ]; then
	echo $username [$vk_user] "is OFFLINE now" | cowsay -f tux | wall
else 
	echo $username [$vk_user] "is ONLINE now" | cowsay -f tux | wall
fi

while [ true ]; do
	cur_status=$(is_user_online $vk_user)

	if [ $cur_status != $status ]; then 
		status=$cur_status

		if [ $cur_status == false ]; then
			notify-send "$username [$vk_user] gone OFFLINE"
			echo $username [$vk_user] "has gone OFFLINE" | cowsay -f tux | wall
		else
			notify-send "$username [$vk_user] back ONLINE"
			echo $username [$vk_user] "is back ONLINE" | cowsay -f tux | wall
		fi
	fi

	sleep $sleep_t
done
