get_am_status() {
	curl -s "api.vk.com/.../fields=online" | grep -c '"online":1' # api call!
}

solve_am() {
	prev_status=$(cat status)
	cur_statur=$(get_am_status)

	if [ "$cur_status" = "$prev_status" ]
	then
		exit
	fi

	if [ "$cur_status" = "1" ]
	then
		<<< "AM is online" wall 2>/dev/null
	else
		<<< "AM is offline" wall 2>/dev/null
	fi

	echo "$cur_status" > status
}

echo "2" > status
export -f get_am_status # function export for inner shell (see below "bash -c solve_am")
export -f solve_am
watch -n 10 "bash -c solve_am"