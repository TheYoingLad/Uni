#!/bin/sh
if [ $# -lt 1 ]
then
    echo "missing parameter"
    echo "-lista: lists the rooms"
    echo "-felismer <face>: returns the name and details of the person with the given face"
    echo "-bemehet <face> <room>: returns whether the given person has permitions for the given room"
    return 1
fi
case $1 in
-lista)
    echo "rooms:"
    cat belepteto.dat | cut -d"," -f1
    ;;
-felismer)
    if [ $# -ne 2 ]
    then
        echo "invalid face"
        return 1
    fi
    arc=`cat user.dat | grep -E "(^|,)$2[,;]"`
    if [ -z "$arc" ]
    then
        echo "no person with face $2 found"
        return 1
    fi
    echo -n "name: "
    echo $arc | cut -d";" -f2
    echo -n "group: "
    echo $arc | cut -d";" -f3
    echo -n "access level: "
    echo $arc | cut -d";" -f4
    ;;
-bemehet)
    if [ $# -ne 3 ]
    then
        echo "invalid face or room"
        return 1
    fi
    room=`cat belepteto.dat | grep -E "$3,"`
    arc=`cat user.dat | grep -E "(^|,)$2[,;]"`
    if [ -z "$room" ]
    then
        echo "this room doesn't exist"
        return 1
    elif [ -z "$arc" ]
    then
        echo "this person doesn't exist"
        return 1
    fi
    rlvl=`echo "$room" | tail -c 2 | head -c 1`
    alvl=`echo "$arc" | tail -c 4 | head -c 1`
    if [ "$alvl" = 'A' ]
    then
        echo "access granted"
        return 0
    fi
    if [ "$alvl" = 'B' -a \( "$rlvl" = 'B' -o "$rlvl" = 'C' \) ]
    then
        echo "access granted"
        return 0
    fi
    if [ "$alvl" = 'C' -a "$rlvl" = 'C' ]
    then
        echo "access granted"
        return 0
    fi
    echo "access denied"
    ;;
*)
    echo "invalid parameter"
    return 1
    ;;
esac