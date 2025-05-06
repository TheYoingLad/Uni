function [B,c,s] = forgat_jo(A,i,j)
    n=size(A,1);
    u=2*A(i,j);
    v=A(j,j)-A(i,i);
    w=sqrt(u^2+v^2);
    c=sqrt((w+v*sign(u))/(2*w));
    s=(u*sign(u))/(2*c*w);
    G=A;
    for k=1:n
        G(i,k)=c*A(i,k)-s*A(j,k);
        G(j,k)=s*A(i,k)+c*A(j,k);
    end
    B=G;
    for k=1:n
        B(k,i)=c*G(k,i)-s*G(k,j);
        B(k,j)=s*G(k,i)+c*G(k,j);
    end
    %B(i,j)=0;
    %B(j,i)=0;
end