#!/bin/sh

# Ellenőrzi, hogy a szkriptnek egy paramétere van-e
if [ $# -ne 1 ]; then
    echo "Hiba: Egy fájlnevet kell megadni paraméterként!" >&2
    exit 1
fi

# Ellenőrzi, hogy a megadott paraméter egy létező fájl-e
if [ ! -r "$1" ]; then
    echo "Hiba: A megadott paraméter nem egy olvasható fájl!" >&2
    exit 2
fi

while IFS=" " read -r first second last; do
    echo "$last" "$second" "$first"
done < "$1"

# cat "$1" | awk '{print $3,$2,$1}'
