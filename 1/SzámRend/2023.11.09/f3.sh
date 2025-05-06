#!/bin/sh
if [$# -lt 1]
then
    echo "no file given"
    return 0
fi
cat $1 | grep -Ev "$2" > out.txt