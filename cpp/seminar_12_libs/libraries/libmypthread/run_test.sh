#!/bin/bash

export LD_LIBRARY_PATH=./bin
export LD_PRELOAD=./bin/libmypthread.so

./bin/test_bench 10
