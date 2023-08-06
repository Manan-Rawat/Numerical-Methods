program quadrature
    implicit none
    integer i,j,k
    real*8 h,a,b,sum,n,t,integ,integ1,diff
    n=20
    j=n
    sum=0
    h=1/n
    do i=1,j-1
        t=(i*h)
        sum=sum+((exP(t)-1)/(100*(t**(0.99))))
    enddo
    do i=1,30
        n=n*2
        h=(1.d0/n)*1
        j=n
        do k=1,j-1,2
            t=(h*k)
            sum=sum+((exP(t)-1)/(100*(t**(0.99))))
        enddo
        integ=(h*sum)
        diff=abs(integ-integ1)
        if (diff<0.000000000001) then
            print *,"CONVERGES"
            exit
        endif
        integ1=integ
        print *,integ+1
    enddo
    print *,integ,"is the final integral", "  the difference is", diff
end program quadrature