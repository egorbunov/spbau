#/bin/bash

max=$(sed '2!d' $1)
rnd_index=$(( RANDOM % max ))
echo $rnd_index
fline=$(echo $1 | grep -nr "$rnd_index")
echo $fline
# sed -n 1,2p $1