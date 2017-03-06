#! /bin/bash

echo $1 | grep -P '^-?(\d+(:?,\d{3})*)(?:\.\d*)?(?:e\d+)?$'