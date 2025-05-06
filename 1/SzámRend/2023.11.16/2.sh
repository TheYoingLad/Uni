#!/bin/sh
s=''
if [ $# -eq 0 ]
then
    be=`cat`
    n=`echo $be | wc -w`
    for i in `seq $n`
    do
        new=`echo $be | cut -d" " -f$i | sed -E 's/(.)(.*)/\2/'`
        s="$s $new"
    done
else
    for i in $*
    do
        new=`echo $i | sed -E 's/(.)(.*)/\2/'`
        s="$s $new"
    done
fi
echo $s