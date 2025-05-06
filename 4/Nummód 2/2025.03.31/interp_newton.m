function yy = interp_newton(C,x,xx)
%UNTITLED2 Summary of this function goes here
%   Detailed explanation goes here

n=length(x);
nn=length(xx);
yy=C(n,n)*ones(1,nn);

for k=n-1:-1:1
    yy=yy.*(xx-x(k))+C(k,k);
end

end