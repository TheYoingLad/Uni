#!/bin/sh

# Ellenőrzi, hogy a szkriptnek egy paramétere van-e
if [ $# -ne 1 ]; then
    echo "Hiba: Egy fájlt kell megadni paraméterként!" >&2
    exit 1
fi

# Ellenőrzi, hogy a megadott paraméter egy létező fájl-e
if [ ! -r "$1" ]; then
    echo "Hiba: A megadott paraméter nem egy olvasható fájl!" >&2
    exit 2
fi

# Szétválogatja a sorokat és írja az eredményt a megfelelő fájlokba
while read -r line; do
    if echo "$line" | grep -iwq "a"; then
        echo "$line" >> a.txt
    else
        echo "$line" >> tobbi.txt
    fi
done < "$1"
