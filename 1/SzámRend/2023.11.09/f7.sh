#!/bin/sh
if [$# -lt 1]
then
    echo "no file given"
    return 0
fi
cat $1 | |sed -E 's/([a-zA-Z]+)( +)([a-zA-Z]+)/\3\2\1/'