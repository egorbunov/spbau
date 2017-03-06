#! /bin/bash

# Binary tree generator

# Repeats given string; takes 2 args: 
# 	1. str to repeat
#   2. number - number of repeats
function repeatStr {
	if [[ $2 -gt 0 ]]; then
		printf -- "$1%.0s" $(seq 1 $2)
	fi
}

# takes 1 arg: prefix size
function printEdge {
	repeatStr " " $1
	repeatStr "|" 1
	repeatStr " " $1
}

# takes 1 arg: prefix size
function printNode {
	let "x=$1-1"
	if [[ "$x" -gt "0" ]]; then
		repeatStr "+" 1
		repeatStr "-" $x
	fi
	repeatStr "o" 1
	if [[ "$x" -gt "0" ]]; then
		repeatStr "-" $x
		repeatStr "+" 1
	fi

}

# takes 1 arg: tree height
function printReversedBinaryTree {
	let "step = 3"
	let "shift = 0"
	let "nodeNumber = 2**$1"
	let "twopow = 1"

	while [[ "$nodeNumber" != "0" ]]; do
		let "nodeNumber /= 2"
		let "prefixSize = twopow"
		if [[ "$twopow" == "1" ]]; then
			let "prefixSize = 0" 
		fi

		if [[ $nodeNumber -gt 0 ]]; then
			repeatStr " " $shift
			for i in $(seq 1 $((nodeNumber-1))); do
				printNode $prefixSize
				repeatStr " " $step
			done
			printNode $prefixSize
			repeatStr " " $shift
			echo ""
			repeatStr " " $shift
			for i in $(seq 1 $((nodeNumber-1))); do
				printEdge $prefixSize
				repeatStr " " $step
			done
			printEdge $prefixSize
			repeatStr " " $shift
			echo ""
		fi

		if [[ $twopow != 1 ]]; then
			let "shift = $shift + $twopow"
			let "step = $step + 2 * $twopow"
		fi
		let "twopow *= 2"
	done
}

# repeatStr " " $1
printReversedBinaryTree $1 | sed '1!G;h;$!d' 
