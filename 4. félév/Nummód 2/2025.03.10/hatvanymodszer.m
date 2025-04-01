function [lambda,v] = hatvanymodszer(A,x0,N)
%Hatványmódszserrel közelítést ad N lépésben A legnagyobb absz értékű
%sajátértékére és a hozzá tartozó sajátvaktorára

[n,m] = size(A);
if(n ~= m)
    error("A nem négyzetes mátrix")
end
m = size(x0);
if(n ~= m)
    error("A és x0 dimenziói nem egyeznek meg")
end
if(N <= 0)
    error("N nem pozitív")
end

x_regi = x0;
x_uj = x0;
sym = issymmetric(A);
for k = 1:N
    x_uj = A*x_regi;
    if(sym)
        lambda = dot(x_uj,x_regi)/dot(x_regi,x_regi);
    else
        [~, i] = max(abs(x_regi));
        lambda = x_uj(i)/x_regi(i);
    end
    x_regi = x_uj/norm(x_uj);
end
v = x_regi;
end