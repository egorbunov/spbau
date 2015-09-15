#! /bin/bash
dd if=/dev/urandom bs=1 count=$RANDOM > rnd.txt
# bs -- block size, count -- number of blocks
