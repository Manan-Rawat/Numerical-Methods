program q4
    implicit none
    real*8,dimension(30,31)::C
    integer i,j
    real*8::x,sum
    C=0
    C(1,1)=(-1)
    C(1,2)=1
    do j=2,30
        do i=2,j+1
            C(j,i)=C(j-1,i-1)-(j*(C(j-1,i)))
        enddo
        C(j,1)=(-j)*(C(j-1,1))
    enddo
    
    print *,(C(30,i), i=1,31)
    
    do i=0,320
        x=-0.05+(0.1*i)
        sum=C(30,31)
        do j=30,1,-1
            sum=(sum*x)+(C(30,j))
        enddo
        print *,x,sum
    enddo
end program q4