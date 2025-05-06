function A = szimm_teszt(n, param)
%random generál nxn-es szimmetrikus mátrixot, aminek elmei [-param, param] között vannak
%alapértelemzett esetben param = 10

if nargin == 1
    param = 10;
end
S = (rand(n) - 1/2) * param; %ennek nem ismerjük a sajátértékeit
D = diag(1:n);

[Q, ~] = qr(S);
A = Q*D*Q';
end