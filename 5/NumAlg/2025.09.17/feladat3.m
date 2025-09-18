%% Probléma
m = 1000;
x = rand(m,1);
y = rand(m,1);

%% Megoldás
A = [ones(m,1),x,y];
[~,R] = qr(A);

B = R(2:3,2:3);
[~,~,V] = svd(B);
n = V(:,2);

c = -(n(1)*R(1,2) + n(2)*R(1,3))/R(1,1);

A = [ones(m,1), x];
p = (A'*A)\(A'*y);
a=p(2);
b=p(1);

%% Megjelenítés
hold on
plot(x,y,".")
plot([0,1],[0,1]*(-n(1)/n(2))+(-c/n(2)), "r")
plot([0,1],[b, a+b], "g")
hold off