#! /bin/bash

# Fibonacci numbers

echo -n "Input not negative int: "
read n
numRe='^[1-9][0-9]*$'
if [[ ! $n =~ $numRe ]] && [[ ! $n == "0" ]]; then
	echo "Bad input =( Script accepts only not negative decimal integers without leading zeros. Exiting..."
	exit 1
fi

let "i=$n"
fa=0
fb=1
while [[ $i != 0 ]]; do
	tmp=$(echo "$fa+$fb" | bc)
	fa=$fb
	fb=$tmp
	let "i-=1"
done
echo "Fib($n) = "
echo "$fa"
