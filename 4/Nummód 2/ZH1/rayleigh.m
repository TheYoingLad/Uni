function [mu,r] = rayleigh(A,N)
%rayleigh 2. tétel

[n,m]=size(A);
if n~=m
    error("A nem négyzetes")
end
if norm(A-A.',2)>=10^(-6)
    error("A nem szimmetrikus")
end

mu=zeros(1,N);
r=zeros(1,N);
for k=1:N
    x=(rand(n,1)-1/2)*2;
    x=x/norm(x,2);
    mu(k)=dot(A*x,x)/dot(x,x);    
    r(k)=norm(A*x-mu(k)*x);
end
plot(mu,r,".")
title("Reziduális hibák")
end