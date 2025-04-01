#!/bin/sh
if [ ! -f aktualis.dat ]
then
    #ez akkor fut le, amikor elindul a gép, azaz nem létezik az aktalis.dat
    cat indulo.dat | awk 'BEGIN {n=1} {print $0 ":" n ":S"; n++;}' > aktualis.dat
fi
if [ $# -lt 1 ]
then
    echo "no parameter given!"
    echo "-start <process name>: starts process with the given name"
    echo "-stop <pid>: terminates process with the given pid, if it exists"
    echo "-kill <pid>: kills process with the given pid, if it exists"
    echo "-lista: lists running and terminated processes"
    return 1
fi
n=`cat aktualis.dat | tail -n 1 | cut -d":" -f2`
n=`expr $n + 1`
case $1 in
-start)
    if [ $# -ne 2 ]
    then
        echo "missing process name"
        return 1
    fi
    valid=`echo $2 | grep -v :`
    if [ -z $valid ]
    then
        echo "invalid process name (':' is not allowed)"
        return 1
    fi
    echo "$2:$n:S" >> aktualis.dat
    n=`expr $n + 1`
    echo "process $2 started"
    ;;
-stop)
    if [ $# -ne 2 ]
    then
        echo "invalid pid"
        return 1
    fi
    rem=`cat aktualis.dat | grep -E ":$2:[ST]"`
    if [ -z "$rem" ]
    then
        echo "no process with pid $2 found"
        return 1
    fi
    rem=`echo "$rem" | cut -d":" -f3`
    if [ $rem = "T" ]
    then
        echo "process with pid $2 already terminated"
        return 1
    fi    
    cat aktualis.dat | sed -E "s/:$2:[ST]/:$2:T/" > tmp
    cat tmp > aktualis.dat
    rm tmp
    echo "process with pid $2 terminated"
    ;;
-kill)
    if [ $# -ne 2 ]
    then
        echo "invalid pid"
        return 1
    fi
    rem=`cat aktualis.dat | grep -E ":$2:[ST]"`
    if [ -z "$rem" ]
    then
        echo "no process with pid $2 found"
        return 1
    fi    
    cat aktualis.dat | grep -vE "^[a-zA-Z0-9]+:$2:[ST]" > tmp
    cat tmp > aktualis.dat
    rm tmp
    echo "process with pid $2 killed"
    ;;
-lista)
    echo -n "current number of processes: "
    cat aktualis.dat | wc -l
    echo "name : pid : status"
    cat aktualis.dat
    ;;
*)
    echo "invalid parameter!"
    return 1
    ;;
esac