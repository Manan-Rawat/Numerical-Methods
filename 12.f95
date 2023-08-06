real*8 function f(x)
    implicit none
    real*8,intent(in)::x
    f=x-(tan(x))
end function f

program roots
    implicit none
    real*8 a,b,c,x,h,ll,ul,mid,f
    integer k,p
    ll=0
    h=0.05
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
            print *,mid,c+1
            c=c+1
        endif
    ll=ul
    ul=ul+h
    if (c==100) exit
    enddo
END program