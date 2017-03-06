#! /bin/bash

ips=$(arp | awk '{print $1}' | tail -n +2)
IFS=$'\n'
for ip in $ips; do
	res=$(ping -c 1 $ip)
	s=$(echo $res | grep "1 packets transmitted, 1 received, 0% packet loss")
	if [ -n $s ]; then
		echo $ip
	fi
done