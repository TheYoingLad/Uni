#!/bin/sh
if [ $# -eq 0 ]
then
    echo "no files given"
    return 1
fi
for i in $*
do
    lc=`echo $i | tr '[A-Z]' '[a-z]'`
    mv $i $lc
done