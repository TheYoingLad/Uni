#!/bin/sh
if [ $# -ne 1 ]
then
    echo "one user as parameter!"
    return 1
fi
while true
do
    sleep 30
    for i in `who | cut -d" " -f1`
    do
        if [ $i = $1 ]
        then
        echo "$1 bejelentkezett!"
        fi
    done
done &