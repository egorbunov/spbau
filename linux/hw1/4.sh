#! /bin/bash
HISTFILE=~/.bash_history
set -o history 

if [ -z $1 ]; then
	echo "Error: no parameters passed! Exiting..."
	exit
fi

# search for command calls in history:
# command may occur at the begining of line, after pipe symbol ('|')
# or like this: $(cmdname ...)
strs=$(history \
	| cut -d " " -f 5- \         # deleting number of command (colums are separated by 2 spaces)
	| sed -e 's/|/\n/g' \        # replacing pipe symbol with new line
	| sed -e 's/^[ \t]*//g' \    # deleting leading spaces
	| sed -e 's/\$(/\n/g' \      # replacing $( with new line
	| grep "^$1 ")               # matching only lines with given command name;
								 # command name must be at the beginning of the line!

# bacause it's possible to match string literal as command call, for example:
# grep " | cat \" " 
# after split and match 'cat'command will be:
# cat \" "
# so we need to check if matched line is correctly quoted
IFS=$'\n'
fin_cnt=0
for str in $strs; do
	bcnt=$(echo $str | grep -o "\\\\\"" | wc -l) # count backslashed quotes
	acnt=$(echo $str | grep -o "\"" | wc -l) # count all quotes
	let quote_cnt=$acnt-$bcnt # fin quote count (without backslashed ones)
	if (( $quote_cnt%2==0 )); then
		let fin_cnt=$fin_cnt+1
	fi
done

echo $fin_cnt
