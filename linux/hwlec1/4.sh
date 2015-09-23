#!/bin/bash
sed -r -e \
's/^[ \t]*#include[ ]?<([^>]*)>[ \t]*/\1/;'\
's/^[ \t]*#include[ ]?"([^"]*)"[ \t]*/\1/' test4.cpp
