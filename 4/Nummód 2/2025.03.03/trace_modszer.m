function p = trace_modszer(A)
%trace módszer a mátrix karakterisztikus polinom meghatározására
%teszt pl: A=[2,3;-1,6], p=trace_modszer(A)

[n,m] = size(A);

%ellenőrzés a méretre
if n ~= m
    error("Nem nxn-es a mátrix")
end

s = zeros(1,n);
p = zeros(1,n);
B = eye(n);

%s vektor feltöltése
for k = 1:n
    B = B * A;
    s(k) = trace(B);
end

%p vektor feltöltése
for k = 1:n
    p(k) = -([1,p(1:k-1)] * s(k:-1:1)')/k;
end

p = [1,p];
end