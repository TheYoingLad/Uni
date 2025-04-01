#!/bin/sh
if [$# -lt 1]
then
    echo "no file given"
    return 0
fi
cat $1 | awk 'BEGIN{s=0} {s+=$1} END{print s}'