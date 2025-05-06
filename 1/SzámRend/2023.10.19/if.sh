#!/bin/sh

if [ 10 -gt $1 ]
then
  echo "kisebb mint 10"
elif [ 10 -lt $1 ]
then
  echo "nagyobb mint 10"
else
  echo "egyenlő 10"
fi
