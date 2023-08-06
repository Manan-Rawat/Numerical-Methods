program bessell
    implicit none
    real*16,dimension(31)::J
    real*16::i
    integer,dimension(3)::L
    integer k,m
    L=(/10,20,30/)
    i=2
    J(1)=0.7651976865
    J(2)=0.4400505857
    do 
        if (i==30) exit
        k=i
        J(k+1)=((2*(i-1))*(J(k)))-J(k-1)
        i=i+1
    enddo
    print *,(J(k), k=1,31)
    do m=1,3
        J=0
        J(L(m))=1.0
        J(L(m)+1)=0.0
        i=L(m)
        do
            if (i==2) exit
                k=i
                J(k-1)=((2*(i-1))*(J(k)))-J(k+1)
                i=i-1
        enddo
        print *,(J(k), k=1,(L(m)+1))
    enddo
        
end program bessell
