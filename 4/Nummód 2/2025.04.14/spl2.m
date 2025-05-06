function P = spl2(x,y,m)
%másodfokú spline interpolálás

n = length(x);
if n ~= length(y)
    error("x és y mérete nem egyezik meg")
end

hold on
P=zeros(n-1, 3);
for k = 1:n-1
    h = x(k+1) - x(k);
    od = (y(k+1)-y(k))/h;

    P(k,3) = y(k);
    P(k,2) = m;
    P(k,1) = (od - m)/h;
    m = 2*od - m;

    xx = linspace(x(k), x(k+1), 25);
    yy = polyval(P(k,:), xx-x(k));
    plot(xx,yy)
end
hold off
end