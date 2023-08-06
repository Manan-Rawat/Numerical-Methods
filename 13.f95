real*8 function f(x)
    implicit none
    real*8,intent(in)::x
    f=(x**6)-((9.2)*(x**5))+(34.45)*(x**4)-(66.914)*(x**3)+(70.684)*(x**2)-(38.168)*(x)+8.112+0.01234*(log(x))
end function f

program roots
    implicit none
    real*8 a,b,c,x,h,ll,ul,mid,f
    integer k,p
    ll=0.05
    h=0.1
    ul=ll+h
    c=0
    do 
        if ((f(ul)*f(ll))<0) then     
            a=ll
            b=ul
            mid=(a+b)/2
            do p=1,100
                mid=(a+b)/2
                if (f(mid)*f(b)>0) then
                    b=mid
                else
                    a=mid
                endif
                if (abs(b-a)<1e-7) exit
            enddo
            print *,mid,c+1,f(mid)
            c=c+1
        endif
    ll=ul
    ul=ul+h
    if (c==6) exit
    enddo
END program