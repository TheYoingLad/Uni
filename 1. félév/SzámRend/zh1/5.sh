#!/bin/sh
echo -n > 5ki.txt
while true
do
    mem=`free -m | grep Mem | awk '{print ($3)*100/($2)}'`
    day=`date | cut -d"," -f1`
    hms=`date | cut -d"," -f3 | cut -d" " -f2`
    echo "$day$hms; $mem%" >> 5ki.txt
    sleep 10
done