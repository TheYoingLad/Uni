m = 10
%% 
x = rand(m,1);
y = rand(m,1);
A = [ones(m,1), x];

%p = polyfit(x,y,1);
p = (A'*A)\(A'*y);

a=p(2);
b=p(1);
%%
plot(x,y,"x");
hold on
plot([0,1],[b, a+b])
hold off