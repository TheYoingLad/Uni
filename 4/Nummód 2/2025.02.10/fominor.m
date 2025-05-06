function d = fominor(A)
%Az A mátrix főminorait egy vektorba
[n,m] = size(A);
d=zeros(1,n);
for k=1:n
    d(k) = det(A(1:k,1:k));
end
end