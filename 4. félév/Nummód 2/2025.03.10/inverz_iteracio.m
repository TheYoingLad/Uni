function [lambda,v] = inverz_iteracio(A,x0,N,p)
%Hatványmódszserrel közelítést ad N lépésben A legkisebb absz értékű
%sajátértékére és a hozzá tartozó sajátvaktorára

if ~exists('p', 'var')
    p = 0;
end
[n,m] = size(A);
if(n ~= m)
    error("A nem négyzetes mátrix")
end
if(det(A) == 0)
    error("A nem invertálató")
end
m = size(x0);
if(n ~= m)
    error("A és x0 dimenziói nem egyeznek meg")
end
if(N <= 0)
    error("N nem pozitív")
end

Ap = A-p*eye(n)
x_regi = x0;
x_uj = x0;
sym = issymmetric(A);
for k = 1:N
    x_uj = A\x_regi;
    if(sym)
        lambda = dot(x_regi,x_regi)/dot(x_uj,x_regi);
    else
        [~, i] = max(abs(x_regi));
        lambda = x_regi(i)/x_uj(i);
    end
    x_regi = x_uj/norm(x_uj);
end
lambda = lambda + p;
v = x_regi;
end