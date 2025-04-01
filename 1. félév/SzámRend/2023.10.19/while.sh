#!/bin/sh

x=0
while [ $x -lt 10 ]
 do
  echo $x
  x=`expr $x + 1`
 done
