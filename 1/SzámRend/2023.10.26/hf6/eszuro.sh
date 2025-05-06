#!/bin/sh
if [ $# -lt 2 ]
then
    echo "no parameters or files given"
    return 1
fi
mode=$1
shift
case $mode in
-l)
    for i in $*
    do
        lc=`echo $i | tr '[A-Z]' '[a-z]'`
        mv $i $lc
    done
    ;;
-u)
    for i in $*
    do
        uc=`echo $i | tr '[a-z]' '[A-Z]'`
        mv $i $uc
    done
    ;;
*)
    echo "wrong parameter"
    ;;
esac
