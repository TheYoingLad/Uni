#!/bin/sh

getent passwd | grep ':666:' | cut -d":" -f5 | tr -s '\n' > torolt_felhasznalok.txt
echo "torolt_felhasznalok.txt állomány létrehozva!"