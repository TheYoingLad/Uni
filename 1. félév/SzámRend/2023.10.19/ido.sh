#!/bin/sh

h=`date +%H`
if [ $h -lt 10 ]
then
  echo "jó reggelt!"
elif [ $h  -lt 18 ]
then
  echo "jó napot!"
else
  echo "jó estét!"
fi
