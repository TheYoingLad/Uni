function C = osztottdiff(x,y)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here

n=length(x);
C=zeros(n,n);
C(:,1)=y'; %első oszlopba y, ami sorvektor

for k=2:n
    for i=k:n
        C(i,k)=(C(i,k-1)-C(i-1,k-1))/(x(i)-x(i-k+1));
    end
end

end