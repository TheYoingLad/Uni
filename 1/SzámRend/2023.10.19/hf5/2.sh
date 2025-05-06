#!/bin/sh
sum=0
for i in `seq 5`
    do
        echo -n "$i. number: "
        read r
        sum=`expr $sum + $r`
    done
echo "sum of these numbers: $sum"
