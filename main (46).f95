program eign
    implicit none
    real*8,dimension(4,4)::h,l
    real*8,dimension(4)::g,x
    real*8,dimension(4)::v,u
    real*8,dimension(4)::t
    integer i,j,k
    real*8 a,b,hi,p,t1
    h=0
    g=1
    I=0
    do i=1,4
        a=i
        do j=1,4
            b=j
            h(i,j)=1/(a+b-1)
        enddo
    enddo
    p=0
    do i=1,20000
        v=matmul(h,g)-p*g
        k=maxloc(abs(v),dim=1)
        g=v/v(k)
        if ((abs(hi-v(k)))<1e-15) exit
        hi=v(k)
    enddo
    print *,hi+p,i,g
    b=hi+p
    t=(/g(2),-g(1),0d-8,0d-8/)
    t1=p
    do i=1,20000
        p=t1
        if (mod(i,5)==0) p=b
        v=matmul(h,t)-p*t
        k=maxloc(abs(v),dim=1)
        t=v/v(k)
        if ((abs(hi-v(k)))<1e-12) exit
            l=v(k)
        hi=v(k)
    enddo
    print *,hi+p,i,t
    
end program eign