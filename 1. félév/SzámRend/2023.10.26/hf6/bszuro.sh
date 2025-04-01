#!/bin/sh
num=0
if [ $# -eq 0 ]
then
    num=`cat`
else
    num=$1
fi
fact=1
for i in `seq 2 $num`
    do
        fact=`expr $fact \* $i`
    done
echo "factorial $1 = $fact"