#! /bin/bash
sort < 8.txt | uniq | sort --random-sort | cat -n

# better solution
sort -uR | cat -n