#!/bin/sh
if [ $# -lt 1 ]
then
    echo "no parameter given!"
    echo "-lista: lists the contents of FAT.dat"
    echo "-szabad: number of free blocks (not occupied)"
    echo "-max: length of the longest continuous sequence of free blocks"
    echo "-foglal <number>: tries to occupy continuous sequence of free blocks with the given length"
    return 1
fi
n=`cat FAT.dat | wc -c`
case $1 in
-lista)
    cat FAT.dat
    echo
    ;;
-szabad)
    db=0
    for i in `seq $n`
    do
        if [ `cat FAT.dat | head -c $i | tail -c 1` = S ]
        then
            db=`expr $db + 1`
        fi
    done
    echo "number of free blocks: $db"
    ;;
-max)
    db=0
    max=0
    for i in `seq $n`
    do
        if [ `cat FAT.dat | head -c $i | tail -c 1` = S ]
        then
            db=`expr $db + 1`
        else
            db=0
        fi
        if [ $db -gt $max ]
        then
            max=$db
        fi
    done
    echo "length of the longest continuous sequence of free blocks: $max"
    ;;
-foglal)
    if [ $# -ne 2 ]
    then
        echo "missing parameter"
        return 1
    fi
    isnum=`echo $2 | grep [[:alpha:]]`
    if [ -n $isnum ]
    then
        echo "$2 is not a number"
        return 1
    fi
    if [ $2 -lt 1 ]
    then
        echo "invalid number!"
        return 1
    fi
    stringS=""
    for i in `seq $2`
    do
        stringS=$stringS"S"
    done
    stringF=`echo $stringS | tr "S" "F"`
    van=`cat FAT.dat | grep "$stringS"`
    if [ -z $van ]
    then
        echo "no continuous sequence of blocks with length of at least $2 found"
        return 1
    fi
    cat FAT.dat | sed "s/$stringS/$stringF/" > tmp
    cat tmp > FAT.dat
    rm tmp
    ;;
*)
    echo "invalid parameter"
    return 1
    ;;
esac