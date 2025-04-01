#!/bin/sh
go=0
while [ $go -eq 0 ]
do
    echo "\n1: logged in users\n2: add 5 numbers\n3: exit"
    read be
    case $be in
        1)
            ./1.sh -w
            ;;
        2)
            ./2.sh
            ;;
        3)
            go=1
            ;;
        *)
            echo "invalid input!"
    esac
done