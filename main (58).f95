function f(x,a)
    real*8 f,x,a
    f=x-(a*sin(x))
end function f

program golden
    implicit none
    real*8 b,c,d,e,l,m,n,g,h,f,y,min,mm
    real*8,dimension(3)::A
    integer i,j,k,x
    g=0.381966
    h=1d-3
    min=1d+7
    A=(/10.0,20.0,20.3959/)
    do j=1,3
        y=A(j)
        do i=1,100
            b=i*h
            c=b+h
            d=i+(2*h)
            if (((f(c,y))<f(b,y)) .and. ((f(c,y))<f(d,y))) then
                do k=1,10
                    if ((d-c)>(c-b)) then
                        e=d-(g*(d-b))
                        if (f(e,h)<f(c,h)) then
                            b=c
                            c=e
                        else
                            d=e
                        endif
                    else
                    e=b+(g*(d-b))
                        if (f(e,y)<f(c,y)) then
                            d=c
                            c=e
                        else
                            b=e
                        endif
                    endif
                    if (abs(((b-d)/d))<1d-3) exit
                enddo
            print *,c,y,f(c,y)
            if (min>(f(c,y))) then
                min=f(c,y)
                mm=c
            endif
            endif
        enddo
        print *,"GLOBAL MINIMA = ",min," X FOR WHICH = ",mm," for a = ",y
        min=1d+7
    enddo
    
end program golden