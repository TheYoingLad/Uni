#!/bin/sh
if [ $# -ne 1 ]
then
    echo "too many/few parameters given! this script only accepts 1!"
    return 1
fi
cat $1 | sed -E -e 's/_//g' -e 's/ //g' -e 's/at/@/g' -e 's/kukac/@/g' -e 's/pont/\./g' -e 's/dot/\./g' -e 's/nospam/@/g' -e 's/dash/-/g' -e 's/kotojel/-/g' -e 's/nospam/@/g' | tee ki.txt
echo | tee -a ki.txt