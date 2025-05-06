#!/bin/sh
if [$# -lt 1]
then
    echo "no file given"
    return 0
fi
cat $1 | grep -E '\b[+-]?(0|([1-9][0-9]*))(\.[0-9]+)?\b'