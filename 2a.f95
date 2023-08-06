real*8 function f(x)
    real*8,intent(in)::x
    integer i,j,k,l,p,d
    real*8 m,h,a,sum,n,t,integ,fin,ul,ll,ln,b,mid
            sum=0
            n=20
            j=n
            h=(1/n)*(60)
            ll=0
            do i=1,j-1  
                t=ll+(i*h)
                sum=sum+((t**2)/(1+(exp((sqrt(t**(2)+1))-x))))
            enddo
            do i=1,20
                n=n*2
                h=(1.d0/n)*(60)
                j=n
                do k=1,j-1,2
                    t=(h*k)+ll
                    sum=sum+((t**2)/(1+(exp((sqrt(t**(2)+1))-x))))
                enddo
                ln=integ
                integ=(h*sum)
                if (abs(ln-integ)<1d-10) exit
            enddo
            f=integ-1d+3
end function f


program quadrature
    implicit none
    integer i,j,k,l,p,d
    real*8 m,h,a,sum,n,t,integ,fin,ul,ll,x,ln,b,mid,f
    l=50
    ln=0
    p=1
    a=-20
    b=+20
    do
        mid=(a+b)/2
        if (f(mid)>0) then
            a=a
            b=mid
            mid=(a+b)/2
        else
            b=b
            a=mid
            mid=(a+b)/2
        endif
        if (abs(f(mid))<1d-8) exit
        print *,mid,f(mid),a,b
    enddo
END program quadrature    