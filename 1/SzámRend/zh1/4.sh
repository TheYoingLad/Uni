#!/bin/sh
if [ ! -r $1 ]
then
    echo "Hiba: $1 fájl nem olvasható" >&2
    exit 1
fi
db=`cat $1 | grep 1 | wc -l`
echo "$db tanulónak van elégtelenje"