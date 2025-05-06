function [kp,r_sor,r_oszl] = gersgorin(A)
%GERSGORIN Kirajzolja az adott A mátrix soraihoz és oszlopaihoz tartozó Gergsgorin köröket
%   Bemenet: A n*n-es mátrix
%   Kimenet: kp: a körök középpontja
%            r_sor,r_oszl: a körök sugara a sorokra és oszlopokra tekintve

%méret ellenőrzés
[n,m]=size(A);
if n ~= m
    error("A mátrix nem n*n-es")
end

%középpontok és sugarak kiszámítása
kp=diag(A);
r_sor=zeros(1,n);
r_oszl=zeros(1,n);
for k=1:n
    r_sor(k)=sum(abs(A(k,:)))-abs(A(k,k));
    r_oszl(k)=sum(abs(A(:,k)))-abs(A(k,k));
end

%kirajzolás
axis equal
grid on
hold on
if issymmetric(A) %ha A szimmetrikus akkor valósak a saját értékei
    for k=1:n
        plot([kp(k)-r_sor(k),kp(k)+r_sor(k)],[0,0],'r-','LineWidth',3)
        plot([kp(k)-r_oszl(k),kp(k)+r_oszl(k)],[1,1],'b-','LineWidth',3)
    end
else
    xx=linspace(0,2*pi,100);
    for k=1:n
        fill(real(kp(k))+abs(r_sor(k))*cos(xx),imag(kp(k))+abs(r_sor(k))*sin(xx),'r','FaceAlpha',0.3)
        fill(real(kp(k))+abs(r_oszl(k))*cos(xx),imag(kp(k))+abs(r_oszl(k))*sin(xx),'b','FaceAlpha',0.3)
    end
end
hold off
end