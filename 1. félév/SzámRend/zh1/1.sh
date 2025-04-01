#!/bin/sh
if [ $# -ne 4 ]
then
    echo "Hiba: nem 4 paramétert adtál meg!" >&2
    exit 1
fi
minta='^[+-]?((0$)|([1-9][0-9]*$))'
a=`echo $1 | tr -d '+'`
b=`echo $2 | tr -d '+'`
c=`echo $3 | tr -d '+'`
d=`echo $4 | tr -d '+'`
if [ -z `echo $a | grep -E $minta` ]
then
    echo "Hiba: nem minden paraméter egész szám!" >&2
    exit 2
elif [ -z `echo $b | grep -E $minta` ]
then
    echo "Hiba: nem minden paraméter egész szám!" >&2
    exit 2
elif [ -z `echo $c | grep -E $minta` ]
then
    echo "Hiba: nem minden paraméter egész szám!" >&2
    exit 2
elif [ -z `echo $d | grep -E $minta` ]
then
    echo "Hiba: nem minden paraméter egész szám!" >&2
    exit 2
fi
ab=`expr $a + $b`
cd=`expr $c \* $d`
abcd=`expr $ab - $cd`
echo "$a + $b - $c * $d = $abcd"