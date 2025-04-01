#!/bin/sh

echo -n "$(($1 + 1)) = "
expr  $1 + 1

echo -n "$(($1 * 2)) = "
expr $1 \* 2
