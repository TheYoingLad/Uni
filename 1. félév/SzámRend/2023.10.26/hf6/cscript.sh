#!/bin/sh
if [ $# -gt 10 -o $# -lt 1 ]
then
    echo "at least 1, at most 10 parameters!"
    exit 1
fi
if [ $1 = "-help" ]
then
    echo "adds the given parameters together (max 10 params)"
    exit 0
fi
sum=0
for i in $*
do
    sum=`expr $sum + $i`
done
echo "sum = $sum"