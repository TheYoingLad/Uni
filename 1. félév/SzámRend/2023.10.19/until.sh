#!/bin/sh

x=0
until [ ! $x -lt 10 ]
 do
  echo $x
  x=`expr $x + 1`
 done
