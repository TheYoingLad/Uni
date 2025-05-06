#!/bin/sh
if [ $# -ne 1 ]
then
    echo "only one file as parameter!"
    return 1
fi
n=0
echo -n > even_lines.txt
echo -n > odd_lines.txt
while read line
do
    if [ `expr $n % 2` -eq 0 ]
    then
        echo $line >> even_lines.txt
    else 
        echo $line >> odd_lines.txt
    fi
    n=`expr $n + 1`
done < "$1"
cat "$1"
printf "\n\n"
cat even_lines.txt
echo
cat odd_lines.txt