#! /bin/bash
git clone https://github.com/OpenCorpora/opencorpora
cd opencorpora
git checkout master
rm -r .git
grep -r -l "token" --include \*.php . | xargs sed -i "1i\/\/ This file contains 'token'"
