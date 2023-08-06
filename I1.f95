program quadrature
    implicit none
    integer i,j,k
    real*8 h,a,b,sum,n,t,integ,integ1,diff
    n=10
    j=n
    sum=0
    h=1/n
    do i=1,j-1
        t=i*h
        sum=sum+(2*exp(-5*t*t)*cos(10*t*t))
    enddo
    do i=1,30
        n=n*2
        h=(1.d0/n)
        j=n
        do k=1,j-1,2
            t=0+(h*k)
            sum=sum+(2*exp(-5*t*t)*cos(10*t*t))
        enddo
        integ=(h*sum)
        diff=abs(integ-integ1)
        if (diff<0.000000000001) then
            print *,"CONVERGES"
            exit
        endif
        integ1=integ
        print *,integ
    enddo
    print *,integ,"is the final integral", "  the difference is", diff
end program quadrature
        