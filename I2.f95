program quadrature
    implicit none
    integer i,j,k
    real*8 h,a,b,sum,n,t,integ,integ1,diff,ff
    n=10
    ff=0
    j=n
    sum=0
    h=1/n
    do i=1,j-1
        t=0.1+i*h
        sum=sum+(1/(t-sin(t)))
    enddo
    do i=1,20
        n=n*2
        h=(1.d0/n)*(0.9)
        j=n
        do k=1,j-1,2
            t=0.1+(h*k)
            sum=sum+(1/(t-sin(t)))
        enddo
        integ=(h*sum)
        diff=abs(integ-integ1)
        if (diff<0.0000001) then
            print *,"CONVERGES"
            exit
        endif
        integ1=integ
        print *,integ
    enddo
    ff=integ+ff
    print *,ff,"is the final integral", "  the difference is", diff
    n=10
    j=n
    sum=0
    h=1/n
    do i=1,j-1
        t=0.001+i*h
        sum=sum+(1/(t-sin(t)))
    enddo
    do i=1,20
        n=n*2
        h=(1.d0/n)*(0.099)
        j=n
        do k=1,j-1,2
            t=0.001+(h*k)
            sum=sum+(6/(t**3))
        enddo
        integ=(h*sum)
        diff=abs(integ-integ1)
        if (diff<0.0000001) then
            print *,"CONVERGES"
            exit
        endif
        integ1=integ
        print *,integ
    enddo
    ff=integ+ff
    print *,ff,"is the final integral", "  the difference is", diff
    n=10
    j=n
    sum=0
    h=1/n
    do i=1,j-1
        t=0.001+i*h
        sum=sum+(1/(t-sin(t)))
    enddo
    do i=1,20
        n=n*2
        h=(1.d0/n)*(0.099)
        j=n
        do k=1,j-1,2
            t=0.001+(h*k)
            sum=sum+(6/(t**3))
        enddo
        integ=(h*sum)
        diff=abs(integ-integ1)
        if (diff<0.000000001) then
            print *,"CONVERGES"
            exit
        endif
        integ1=integ
        print *,integ
    enddo
    ff=integ+ff
    print *,ff,"is the final integral", "  the difference is", diff
    n=10
    j=n
    sum=0
    h=1/n
    do i=1,j-1
        t=0.000000001+i*h
        sum=sum+(6/(t**3))
    enddo
    do i=1,20
        n=n*2
        h=(1.d0/n)*(0.00000099)
        j=n
        do k=1,j-1,2
            t=0.000000001+(h*k)
            sum=sum+(1/(t-sin(t)))
        enddo
        integ=(h*sum)
        diff=abs(integ-integ1)
        if (diff<0.0000000001) then
            print *,"CONVERGES"
            exit
        endif
        integ1=integ
        print *,integ
    enddo
    ff=integ+ff
    print *,ff,"is the final integral"
end program quadrature
        