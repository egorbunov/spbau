#! /bin/bash

jobfile=$(readlink -f "5job.sh")

crontab -l -u "$USER" > tmpcronconf
if [[ $(cat tmpcronconf) == "no crontab for $USER" ]]; then
	echo "" > tmpcronconf
fi
echo "* * * * * bash \"$jobfile\"" >> tmpcronconf
crontab tmpcronconf
rm tmpcronconf
