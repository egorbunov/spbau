#!/bin/bash

rev=$(echo $1 | rev)

if [[ $1 == $rev ]]; then
	echo "PALINDROME!"
else
	echo "NOT A PALINDROME!"
fi