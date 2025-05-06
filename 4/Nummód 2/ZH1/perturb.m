function hiba = perturb(n,q,N)
%UNTITLED5 Summary of this function goes here
%   perturb(100,0.95,50)

A=4*eye(n)+diag(ones(n-1,1),1)+diag(ones(n-1,1),-1)+diag(ones(n-2,1),2)+diag(ones(n-2,1),-2);
hiba=zeros(1,N);
for k=1:N
    P=A+(rand(n)-1/2)*2*q^k;
    hiba(k)=norm(eig(A)-eig(P),"inf");
end
plot(1:N,hiba)
end