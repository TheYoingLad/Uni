function [A,x] = teszt1(n)
%A szimm mátrix és x nemnulla véletlen vektort generál

A=diag(1:n);
x=rand(n,1);
end