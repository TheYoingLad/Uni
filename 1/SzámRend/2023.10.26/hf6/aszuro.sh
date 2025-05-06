#!/bin/sh
if [ $# -eq 0 ]
then
    printf "no parameter given\nparameter: -a -d -c -o\n"
    return 1
fi
case $1 in
-a)
    db=`cat telefon.txt | grep "$2:$3" | wc -l`
    if [ $db -eq 0 ]
    then
        printf "$2:$3\n" >> telefon.txt
        echo "entry added to the list"
    else
        echo "entry already in the list!"
    fi
    ;;
-d)
    db=`cat telefon.txt | grep "$2:$3" | wc -l`
    if [ $db -eq 0 ]
    then
        echo "entry not in the list!"        
    else
        cat telefon.txt | grep -v "$2:$3" > tmp
        cat tmp > telefon.txt
        rm tmp
        echo "entry removed from the list"
    fi
    ;;
-c)
    sor=`cat telefon.txt | wc -l`
    echo "number of entries: $sor"
    ;;
-o)
    echo "names in the list:"
    cat telefon.txt | cut -d":" -f1 | sort -u
    ;;
*)
    echo "phone numbers under this name:"
    cat telefon.txt | grep "^$1:" | cut -d":" -f2
    ;;
esac