function [r,h] = rezid(A,x,N)
%2. tétel biz

if x == 0
    error("x nulla vektor")
end
if ~issymmetric(A)
    error("A nem szimmetrikus")
end

r=zeros(1,N);
ainf=norm(A,"inf");
xx=linspace(-ainf,ainf,N);
for k=1:N
    r(k)=norm((A*x-xx(k)*x),2);
end
h=dot(A*x,x)/dot(x,x);
plot(xx,r,".",h,norm((A*x-h*x),2),"r*")
end