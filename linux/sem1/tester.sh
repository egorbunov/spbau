#! /bin/bash

for t in tests/*.test
do
	./task.sh < $t > out.txt
	diff -q out.txt "$t.out"
done
rm out.txt