PROGRAM quadrature
    implicit none
    integer i,j,k,l,p,d,f
    real*8 m,h,a,sum,n,t,integ,fin,ul,ll,x,ln,e
    l=50
    ln=0
    p=1
        do f=1,40
            x=f
            m=p
            sum=0
            n=20
            j=n
            h=(1/n)*(2**(-x))
            ll=2**(-x)
            do i=1,j-1  
                t=ll+(i*h)
                sum=sum+((1/(exp(t)-1))-(exp(-t)/t))
            enddo
            do i=1,20
                n=n*2
                h=(1.d0/n)*(2**(-x))
                j=n
                do k=1,j-1,2
                    t=(h*k)+ll
                    sum=sum+((1/(exp(t)-1))-(exp(-t)/t))
                enddo
                integ=(h*sum)
            enddo
                ln=ln+integ
        enddo
        print *,ln,ll
        do f=1,50
            x=f
            m=p
            sum=0
            n=20
            j=n
            h=(1/n)*(2**(x-1))
            ll=2**(x-1)
            do i=1,j-1  
                t=ll+(i*h)
                sum=sum+((1/(exp(t)-1))-(exp(-t)/t))
            enddo
            do i=1,20
                n=n*2
                h=(1.d0/n)*(2**(x-1))
                j=n
                do k=1,j-1,2
                    t=(h*k)+ll
                    sum=sum+((1/(exp(t)-1))-(exp(-t)/t))
                enddo
                integ=(h*sum)
            enddo
                ln=ln+integ
                print *,ln
        enddo
        print *,ln,ll
    
    
END PROGRAM quadrature    