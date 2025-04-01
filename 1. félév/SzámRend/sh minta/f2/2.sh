#!/bin/sh

# Ellenőrzi, hogy a szkriptnek egy paramétere van-e
if [ $# -eq 1 ]; then
    n=$1
elif [ $# -eq 0 ]; then
    read n
else
    echo "Hiba: Egyetlen egész számot kell megadni paraméterként!" >&2
    exit 1
fi

# Ellenőrzi, hogy a paraméter pozitív egész szám-e
if ! echo $n | grep -q "^[1-9][0-9]*$"; then
    echo "Hiba: Pozitív egész számot kell megadni paraméterként!" >&2
    exit 2
fi

sum=1
end=`expr $n - 1`
for i in `seq 2 $end`; do
    if [ `expr $n % $i` -eq 0 ]; then
        sum=`expr $sum + $i`
    fi
done

# Ellenőrzi, hogy a szám hiányos-e
if [ $sum -lt $n ]; then
    echo "$n hiányos szám."
else
    echo "$n nem hiányos szám."
fi
