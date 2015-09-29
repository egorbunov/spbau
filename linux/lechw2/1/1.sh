#! /bin/bash

echo "Need to get sections table from MBR (/dev/sda). It's root protected, so please, believe me and enter your password!"
sudo dd if=/dev/sda ibs=1 skip=446 count=64 of=tmp_sectors status=none

let "CNT=0"
for i in $(seq 0 16 63);
do
	res=$(dd if=tmp_sectors ibs=1 skip=$i count=16 status=none)
	if [ -n "$res" ]; then
		let "CNT+=1"
	fi
done

echo "Number of sections = $CNT"
sudo rm tmp_sectors