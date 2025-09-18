%% Probléma
A = [10; 15];
B = [3; 4];
r = 0.2;
m = 100;

%% Számolás
v = (B - A) / norm(B - A, 2);
n = [v(2);-v(1)];

t = rand(1,m);
q = rand(1,m)*2-1;

p = A*ones(1,m) + (B-A)*t + r*n*q;

%% Megjelenítés
hold on
axis equal
plot([A(1),B(1)],[A(2),B(2)],"kX", "LineWidth",3);
plot(p(1,:),p(2,:), ".");
hold off