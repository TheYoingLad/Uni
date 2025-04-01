#!/bin/sh

for i in `who | cut -d" " -f1 |sort -u`
 do
  echo "$i be van jelentkezve!"
 done
