program quadrature
    implicit none
    integer i,j,k
    real*8 h,a,b,sum,n,t,integ,integ1,diff
    n=10
    j=n
    sum=0
    a=0.2
    h=0.8/n
    do i=1,j-1
        t=a+i*h
        sum=sum+(exp(1/t))
    enddo
    do i=1,20
        n=n*2
        h=(1.d0/n)*(0.8)
        j=n
        do k=1,j-1,2
            t=a+(h*k)
            sum=sum+(exp(1/t))
        enddo
        integ=(h*sum)+(h/2)*(exp(5.0)+exp(1.0))
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
        