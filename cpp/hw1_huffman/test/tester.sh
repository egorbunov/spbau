#!/bin/bash

BINARY="../bin/huffman"

RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[1;34m'
PURPLE='\033[0;35m'
NC='\033[0m'

find . -regex ".*test[0-9]*\.txt" | sort | while read FILE; do
	CF=$(mktemp) # compressed file name
 	UF=$(mktemp) # uncompressed file name

 	echo "Testing on file [ $FILE ]"

 	echo -n "	Compression... "
 	START=$(date +%s.%N)
	RET=$($BINARY -c -f "$FILE" -o "$CF")
	END=$(date +%s.%N)
	DIFF=0$(echo "$END - $START" | bc)
	echo "$DIFF seconds"
	echo "$RET" | while read X; do
		printf "		$BLUE$X$NC\n"
	done

 	echo -n "	Decompression... "
	START=$(date +%s.%N)
	RET=$($BINARY -u -f "$CF" -o "$UF")
	END=$(date +%s.%N)
	DIFF=0$(echo "$END - $START" | bc)
	echo "$DIFF seconds"
	echo "$RET" | while read X; do
		printf "		$BLUE$X$NC\n"
	done

 	DIFF=$(diff $UF $FILE)
 	if [ ! -z "$DIFF" ]; then
 		printf "${RED}FAILED on file: [ $FILE ]$NC\n"
 	else
 		printf "${GREEN}PASSED on file: [ $FILE ]$NC\n"
 	fi

 	rm "$CF" "$UF"

	echo $f
done
