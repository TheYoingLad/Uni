@echo off
g++ -c .\bank_account.cpp -o b.o
g++ -c .\person.cpp -o p.o
g++ .\driver.cpp .\b.o .\p.o -o driver.exe
del .\*.o
.\driver.exe