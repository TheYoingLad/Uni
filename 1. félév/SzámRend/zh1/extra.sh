#!/bin/sh
echo "1. feladat"

echo "teszt 1:"
echo "be: 1 2 3 4"
echo -n "ki: "
./1.sh 1 2 3 4
err=$?
if [ $err -ne 0 ]
then
    echo "hibakód: $err"
fi

echo "teszt 2:"
echo "be: 1 +2 -3 +40"
echo -n "ki: "
./1.sh 1 +2 -3 +40
err=$?
if [ $err -ne 0 ]
then
    echo "hibakód: $err"
fi

echo "teszt 3:"
echo "be: 1 02 3 4"
echo -n "ki: "
./1.sh 1 02 3 4
err=$?
if [ $err -ne 0 ]
then
    echo "hibakód: $err"
fi

echo "teszt 4:"
echo "be: 1 a 3 4"
echo -n "ki: "
./1.sh 1 a 3 4
err=$?
if [ $err -ne 0 ]
then
    echo "hibakód: $err"
fi

echo "teszt 5:"
echo "be: 1 a 3"
echo -n "ki: "
./1.sh 1 a 3
err=$?
if [ $err -ne 0 ]
then
    echo "hibakód: $err"
fi

################################################
echo 
echo "2. feladat"

echo "teszt 1:"
echo "be: alma fa jaj"
echo -n "ki: "
./2.sh alma fa jaj

echo "teszt 2:"
echo "be: echo huha nagy szuro | ./2.sh"
echo -n "ki: "
echo huha nagy szuro | ./2.sh

################################################
echo 
echo "3. feladat"

echo "teszt 1:"
echo "be: huha.txt"
echo -n "ki: "
./3.sh huha.txt
err=$?
if [ $err -ne 0 ]
then
    echo "hibakód: $err"
fi

echo "teszt 2:"
echo "be: 3szamok.txt"
echo -n "ki: "
./3.sh 3szamok.txt
err=$?
if [ $err -ne 0 ]
then
    echo "hibakód: $err"
fi

################################################
echo 
echo "4. feladat"

echo "teszt 1:"
echo "be: jaj.csv"
echo -n "ki: "
./4.sh huha.csv
err=$?
if [ $err -ne 0 ]
then
    echo "hibakód: $err"
fi

echo "teszt 2:"
echo "be: 4oszt.csv"
echo -n "ki: "
./4.sh 3szamok.txt
err=$?
if [ $err -ne 0 ]
then
    echo "hibakód: $err"
fi

################################################
echo 
echo "5. feladat"

echo "teszt 1:"
echo "be:"
echo -n "ki: (5ki.txt tartalma)"
./5.sh