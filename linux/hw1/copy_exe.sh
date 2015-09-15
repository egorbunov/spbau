#! /bin/bash

FROM=""
TO=""
while (( $#>1 )); do
	key="$1"
	case $key in 
	-i)
		FROM="$2"
		;;
	-o)
		TO="$2"
		;;
	esac
	shift 2
done

if [[ -z $FROM || -z $TO ]]; then
	echo "ERROR: not all arguments specified!"
	exit
else
	FROM=$(readlink -m $FROM)
	TO=$(readlink -m $TO)

	if [[ ! -d "$FROM" || ! -d "$TO" ]]; then
	  echo "ERROR: one or both of the specified directories does not exist!"
	  exit
	fi

	echo Executables from [ $FROM ] will be copied to [ $TO ]
fi

files=$(find $FROM -type f -executable)
(cd $FROM && \
for f in $files; do
	f="${f#$FROM/}"
	echo $f
	cp --parents $f -t $TO
done
)