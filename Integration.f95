    REAL*8 FUNCTION W(x)
        implicit none
        real*8,intent(in)::x
        real*8 s,l,j
        integer i
        s=0
        do i=1,100
            j=i
            !!?? pi should be calculed outside the loop and use atan(1d0)
            l=(7**(j))*(4*x)*(atan(1.0))
            s=s+((2**(-j))*cos(l))
        enddo
        W=s/(atan(1.0)*4)
    end function W

program quadrature
    implicit none
    integer i,j,k
    real*8 h,a,b,sum,n,t,integ,integ1,diff,W,s
    n=10
    j=n
    sum=0
    h=1/n
    do i=1,j-1
        t=i*h
        sum=sum+(t**(t))
    enddo
    do i=1,20
        n=n*2
        h=1.d0/n
        j=n
        do k=1,j-1,2
            t=0+(h*k)
            sum=sum+(t**(t))
        enddo
        integ=(h*sum)+(h)
        diff=abs(integ-integ1)
        if (diff<0.000000000001) then
            print *,"CONVERGES"
            exit
        endif
        integ1=integ
        print *,integ
    enddo
    print *,integ,"is the final integral", "  the difference is", diff
    
    n=10
    j=n
    sum=0
    h=1/n
    do i=1,j-1
        t=i*h
        sum=sum+(exp((-t)-(1/t))/t**(20))
    enddo
    do i=1,20
        n=n*2
        h=1.d0/n
        j=n
        do k=1,j-1,2
            t=0+(h*k)
            sum=sum+(exp((-t)-(1/t))/t**(20))
        enddo
        integ=(h*sum)+(h/2)*(exp(-2.0))
        diff=abs(integ-integ1)
        if (diff<0.000000000001) then
            print *,"CONVERGES"
            exit
        endif
        integ1=integ
        print *,integ
    enddo
    print *,integ,"is the final integral", "  the difference is", diff
    
    n=10
    j=n
    sum=0
    h=1/n
    do i=1,j-1
        t=i*h
        sum=sum+(log(1/t)*sin(t))
    enddo
    do i=1,20
        n=n*2
        h=1.d0/n
        j=n
        do k=1,j-1,2
            t=0+(h*k)
            sum=sum+(log(1/t)*sin(t))
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
        !!?? why exp(-t*t)? It should be exp(-1d0), t will be 1-h
        integ=(h*sum)+(1+exp(-(t*t)))*(h/2)
        diff=abs(integ-integ1)
        !!?? your value of pi is not accurate enough. erf is >1
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
        !!?? because of pi value the integral is not coverging
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
    n=10
    j=n
    sum=0
    h=10/n
    do i=1,j-1
        t=i*h
        sum=sum+(exp(-t*t))
    enddo
    do i=1,20
        n=n*2
        h=10.d0/n
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
    print *,erf(10.0)
    print *,integ,"is the final integral", "  the difference is", diff
    
    n=10
    j=n
    s=0
    sum=0
    a=0.2
    b=0.1
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
    s=s+integ
    print *,integ,"is the final integral", "  the difference is", diff
    n=10
    j=n
    sum=0
    a=0.2
    b=0.1
    h=0.1/n
    do i=1,j-1
        t=0.1+i*h
        sum=sum+(exp(1/t))
    enddo
    do i=1,20
        n=n*2
        h=(1.d0/n)*(0.1)
        j=n
        do k=1,j-1,2
            t=0.1+(h*k)
            sum=sum+(exp(1/t))
        enddo
        integ=(h*sum)+(h/2)*(exp(5.0)+exp(10.0))
        diff=abs(integ-integ1)
        if (diff<0.000000000001) then
            print *,"CONVERGES"
            exit
        endif
        integ1=integ
        print *,integ
    enddo
    s=s+integ
    print *,integ,"is the final integral", "  the difference is", diff
    !!?? n is not reset to 10
    a=0.01
    b=0.1
    !!?? h should be 0.09/n
    h=0.1/n
    do i=1,j-1
        t=0.01+i*h
        sum=sum+(exp(1/t))
    enddo
    do i=1,20
        n=n*2
    !!?? h should be 0.09/n
        h=(1.d0/n)*(0.009)
        j=n
        do k=1,j-1,2
            t=0.01+(h*k)
            sum=sum+(exp(1/t))
        enddo
        integ=(h*sum)+(h/2)*(exp(100.d0)+exp(10.0))
        diff=abs(integ-integ1)
        if (diff<0.000000000001) then
            print *,"CONVERGES"
            exit
        endif
        integ1=integ
        print *,integ
    enddo
    s=s+integ
    print *,integ,"is the final integral", "  the difference is", diff
    !!?? n is not reset to 10 and h is also not defined =0.008/n
    do i=1,j-1
        t=0.002+i*h
        sum=sum+(exp(1/t))
    enddo
    do i=1,20
        n=n*2
        h=(1.d0/n)*(0.0008)
        j=n
        do k=1,j-1,2
            t=0.01+(h*k)
            sum=sum+(exp(1/t))
        enddo
        integ=(h*sum)+(h/2)*(exp(500.d0)+exp(100.d0))
        diff=abs(integ-integ1)
        if (diff<0.000000000001) then
            print *,"CONVERGES"
            exit
        endif
        integ1=integ
        print *,integ
    enddo
    s=s+integ
    print *,integ,"is the final integral", "  the difference is", diff
    print *,"FINAL INTEGRAL", s
    
    n=10
    j=n
    sum=0
    !!?? how do you get this, the range is inifinite
    h=1/n
    do i=1,j-1
        t=i*h
        sum=sum+((t**3)/(exp(t)-1))
    enddo
    do i=1,30
        n=n*2
        h=(1.d0/n)*1000000
        j=n
        do k=1,j-1,2
            t=0+(h*k)
            sum=sum+((t**3)/(exp(t)-1))
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
    
    n=10
    j=n
    sum=0
    h=0.1/n
    do i=1,j-1
        t=i*h+(0.1)
        sum=sum+(W(t))
    enddo
    do i=1,20
        n=n*2
        h=(1.d0/n)*(0.1)
        j=n
        do k=1,j-1,2
            t=0.1+(h*k)
            sum=sum+(W(t))
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
        
