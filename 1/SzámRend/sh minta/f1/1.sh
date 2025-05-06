#!/bin/sh

# Ellenőrzi, hogy a szkriptnek két paramétere van-e
if [ $# -ne 2 ]; then
    echo "Hiba: Két egész számra van szükség paraméterként!" >&2
    exit 1
fi


# Ellenőrzi, hogy a paraméterek pozitív egész számok-e
pattern="^[1-9][0-9]*$"
if ! echo $1 | grep -q "$pattern" || ! echo $2 | grep -q "$pattern"; then
    echo "Hiba: Mindkét paraméternek pozitív egész számnak kell lennie!" >&2
    exit 2
fi

current=$1

while [ $current -le $2 ]; do
    if [ `expr $current % 3` -eq 0 ] && [ `expr $current % 5` -eq 0 ]; then
        echo $current
    fi
    current=`expr $current + 1`
done

