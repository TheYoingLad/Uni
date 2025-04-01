#!/bin/sh
read line
touch temp
until [ $line = "vége" ]
do
    echo $line >> temp
    read line
done
echo
cat temp | sort
rm temp