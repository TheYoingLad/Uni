#!/bin/sh

A=$(echo $* | cut -d" " -f$#)
expr $A + $1
