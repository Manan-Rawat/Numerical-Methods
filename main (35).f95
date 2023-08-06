real*8 function f(x,pi)
    implicit none
    real*8,intent(in)::x,pi
    real*8 y
    integer b
    common y
    y=acos(x/exp(x))+pi
    f=(exp(x)*sin(y))-(y)
end function f

real*8 function g(x,pi)
    implicit none
    real*8,intent(in)::x,pi
    real*8 y
    integer b
    common y
    y=-acos(x/exp(x))+pi
    g=(exp(x)*sin(y))-(y)
end function g

program roots
    implicit none
    real*8 a,b,c,x,h,ll,ul,mid,f,y,pi,g
    integer k,p,l
    common y
    pi=4*atan(1d0)
    pi=-2*pi
    do l=1,3
    ll=-10
    h=1d-3
    ul=ll+h
    c=0
    do 
        if ((f(ul,pi)*f(ll,pi))<0) then     
            a=ll
            b=ul
            mid=(a+b)/2
            do p=1,100
                mid=(a+b)/2
                if (f(mid,pi)*f(b,pi)>0) then
                    b=mid
                else
                    a=mid
                endif
                if (abs(b-a)<1e-7) exit
            enddo
            print *,mid,y,f(mid,pi)
            c=c+1
        endif
    ll=ul
    ul=ul+h
    if (ul>10) exit
    enddo
        pi=pi+8*atan(1d0)
    enddo
    pi=4*atan(1d0)
    pi=-2*pi
    do l=1,3
    ll=-10
    h=1d-3
    ul=ll+h
    c=0
    do 
        if ((g(ul,pi)*g(ll,pi))<0) then     
            a=ll
            b=ul
            mid=(a+b)/2
            do p=1,100
                mid=(a+b)/2
                if (g(mid,pi)*g(b,pi)>0) then
                    b=mid
                else
                    a=mid
                endif
                if (abs(b-a)<1e-7) exit
            enddo
            print *,mid,y,g(mid,pi)
            c=c+1
        endif
    ll=ul
    ul=ul+h
    if (ul>10) exit
    enddo
        pi=pi+8*atan(1d0)
    enddo
END program

