#!/bin/sh

n=$(getent passwd | wc -l)
echo $n > felhasznalok.txt
echo "$n felhasználó adatát kaptuk meg"
