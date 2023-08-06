PROGRAM quadrature
    implicit none
    integer i,j,k,l,p,d
    real*8 m,h,a,sum,n,t,integ,fin,ul,ll,x
    real*8,dimension(5)::B
    B=(/2.0,1.1,1.01,1.001,1.0001/)
    do d=1,5
        x=B(d)
        do p=1,l
            m=p
            sum=0
            n=20
            j=n
            h=(1/n)*(2**(-m))*(x-1)
            ll=1+((2**(-m))*(x-1))
            do i=1,j-1  
                t=ll+(i*h)
                sum=sum+(sqrt(((x*x)-(t*t))/((t*t)-1)))
            enddo
            do i=1,20
                n=n*2
                h=(1.d0/n)*(2**(-m))*(x-1)
                j=n
                do k=1,j-1,2
                    t=(h*k)+ll
                    sum=sum+(sqrt(((x*x)-(t*t))/((t*t)-1)))
                enddo
                integ=(h*sum)
                fin=fin+integ
            enddo
        enddo
        print *,fin
        do p=1,l-1
            m=p
            sum=0
            n=20
            j=n
            h=(1/n)*(2**(-m))*(x-1)
            ll=1
            do i=1,j-1  
                t=ll+(i*h)
                sum=sum+(sqrt(((t**2)-1)/((x*x)-(t*t))))
            enddo
            do i=1,20
                n=n*2
                h=(1.d0/n)*(2**(-m))*(x-1)
                j=n
                do k=1,j-1,2
                    t=(h*k)+ll
                    sum=sum+(sqrt(((t**2)-1)/((x*x)-(t*t))))
                enddo
                integ=(h*sum)
                fin=fin+integ
            enddo
        enddo
        print *,(fin/((x*x)-1)),x
    enddo
end program quadrature        
        
        