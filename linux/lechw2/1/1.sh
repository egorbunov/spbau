#! /bin/bash

echo "Need to get sections table from MBR (/dev/sda). It's root protected, so please, believe me and enter your password!"
sudo dd if=/dev/sda ibs=1 skip=446 count=64 of=tmp_sectors status=none

let "CNT=0"
let "ind=0"
for i in $(seq 0 16 63);
do
	res=$(dd if=tmp_sectors ibs=1 skip=$i count=16 status=none)
	if [ -n "$res" ]; then
		let "CNT+=1"
		let "type_shift=i+4"
		echo "------------------ SECTION with index $ind ----------------- "
	    part_type=$(dd if=tmp_sectors ibs=1 skip=$type_shift count=1 status=none | hexdump -v -e '"0x" 1/1 "%02X" "\n"')
	    cat "part_types.txt" | grep "$part_type"
	fi
	let "ind+=1"
done
echo "----------------------------------------------- "
echo "Number of sections = $CNT"
sudo rm tmp_sectors