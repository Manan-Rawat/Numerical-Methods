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
        sum=sum+(exp(-t*t))
    enddo
    do i=1,20
        n=n*2
        h=1.d0/n
        j=n
        do k=1,j-1,2
            t=0+(h*k)
            sum=sum+(exp(-t*t))
        enddo
        integ=(h*sum)+(1+exp(-(t*t)))*(h/2)
        diff=abs(integ-integ1)
        integ=integ*2/sqrt(3.14159)
        if (diff<0.000000000001) then
            print *,"CONVERGES"
            exit
        endif
        integ1=integ
        print *,integ
    enddo
    print *,erf(1.0)
    print *,integ,"is the final integral", "  the difference is", diff
    n=10
    j=n
    sum=0
    h=5/n
    do i=1,j-1
        t=i*h
        sum=sum+(exp(-t*t))
    enddo
    do i=1,20
        n=n*2
        h=5.d0/n
        j=n
        do k=1,j-1,2
            t=0+(h*k)
            sum=sum+(exp(-t*t))
        enddo
        integ=(h*sum)+(1+exp(-(t*t)))*(h/2)
        diff=abs(integ-integ1)
        integ=integ*2/sqrt(3.14159)
        if (diff<0.000000000001) then
            print *,"CONVERGES"
            exit
        endif
        integ1=integ
        print *,integ
    enddo
    print *,erf(5.0)
    print *,integ,"is the final integral", "  the difference is", diff
end program quadrature
        