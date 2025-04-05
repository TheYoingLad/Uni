function [A,B] = teszt2()
%UNTITLED8 Summary of this function goes here
%   Detailed explanation goes here

A=(rand(5)-1/2)*20;
A=A*A.';

B=diag([1,1:4]);
[Q,~]=qr((rand(5)-1/2)*20);
B=Q*B*Q.';
end