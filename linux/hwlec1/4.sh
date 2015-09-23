#!/bin/bash
sed -r -n -e \
's/^\s*#include[ ]?<([^>]*)>\s*/\1/p;'\
's/^\s*#include[ ]?"([^"]*)"\s*/\1/p' test4.cpp
