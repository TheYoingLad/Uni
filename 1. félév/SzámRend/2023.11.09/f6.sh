#!/bin/sh
if [$# -lt 1]
then
    echo "no file given"
    return 0
fi
cat $1 | sed -E 's/([0-9])([0-9])/\2\1/'