REAL*8 FUNCTION W(x)
        implicit none
        real*8,intent(in)::x
        real*8 s,l,j
        integer i
        s=0
        do i=1,100
            j=i
            l=(7**(j))*(4*x)*(atan(1.0))
            s=s+((2**(-j))*cos(l))
        enddo
        W=s/(atan(1.0)*4)
    end function W
    
program quadrature
    implicit none
    integer i,j,k
    real*8 h,a,b,sum,n,t,integ,integ1,diff,W
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
        