#!/bin/sh
case $1 in
    -d)
        date
        ;;
    -w)
        who
        ;;
    -l)
        ls
        ;;
    *)
        echo "ERROR: incorrect option!\n-d: current date\n-l: list directory contents\n-w: logged in users"
        ;;
esac 