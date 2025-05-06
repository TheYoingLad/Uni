#!/bin/sh
if [ ! -e $1 ]
then
    echo "Hiba: $1 fájl nem létezik" >&2
    exit 1
elif [ ! -r $1 ]
then
    echo "Hiba: $1 fájl nem olvasható" >&2
    exit 2
fi
sum=`cat $1 | awk 'BEGIN{n=0} {if(NR%2==0) n+=$1} END{print n}'`
echo "páros sorok számainak összege: $sum"