PROGRAM quadrature
    implicit none
    integer i,j,k,l,p,d
    real*8 m,h,a,sum,n,t,integ,fin,ul,ll,x,ln,er
    l=50
    ln=0
    p=1
    do 
            m=p
            sum=0
            n=20
            j=n
            h=(1/n)*(2**(m))
            ll=2**(m)
            do i=1,j-1  
                t=ll+(i*h)
                sum=sum+(1/(t*log(t)))
            enddo
            do i=1,20
                n=n*2
                h=(1.d0/n)*(2**(m))
                j=n
                do k=1,j-1,2
                    t=(h*k)+ll
                    sum=sum+(1/(t*log(t)))
                enddo
                integ=(h*sum)
            enddo
                ln=ln+integ
                print *,integ,p,ln,log(m+1)
            p=p+1
            if (p>50) exit
    enddo
    print *,ln
END PROGRAM quadrature    