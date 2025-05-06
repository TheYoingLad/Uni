#!/bin/sh
sum=0
if [ $# -ne 0 ]
then
    if [ $1 = "-help" ]
    then
        echos "adds the given parameters together (max 10 params)"
        exit 0
    fi
    for i in $*
    do
        sum=`expr $sum + $i`
    done
else
    for i in `cat`
    do
        sum=`expr $sum + $i`
    done
fi
echo "sum = $sum"