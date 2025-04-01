#!/bin/sh
if [ $# -ne 0 ]
then
    fact=1
    for i in `seq 2 $1`
        do
            fact=`expr $fact \* $i`
        done
    echo "factorial $1 = $fact"
else
    echo "no parameter given"
fi
