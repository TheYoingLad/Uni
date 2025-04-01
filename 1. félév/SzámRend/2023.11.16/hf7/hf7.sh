#!/bin/sh

cat log.dat | grep "Segmentation fault" | tr -d "." | sed -E 's/ [A-Z]{4,}/;/' | awk -F";" -v OFS= '{print"{\n\"timestamp\": \"",$1,"\",\n\"message\":",$2,"\"\n}"}' | sed -E 's/\"Segmentation fault\"/\\\"Segmentation fault\\\"/g'