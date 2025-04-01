function [D,Q] = forgat_cikl(A,cikl,ep)
%ciklikus jakobi számolsá
n=size(A,1);
Q=eye(n);
B=A;
for c=1:cikl
    for i=1:n-1
        for j=i+1:n
            if abs(B(i,j))>=ep^c
                [B,c,s]=forgat_jo(B,i,j);
                QS=Q;
                for k=1:n
                    QS(k,i)=c*Q(k,i)-s*Q(k,j);
                    QS(k,j)=s*Q(k,i)+c*Q(k,j);
                end
                Q=QS;
            end
        end
    end
end
D=B;
end