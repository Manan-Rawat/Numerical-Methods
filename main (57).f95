function f(x)
    real*8 f,x
    f=((x**6)-9.2*(x**5)+34.45*(x**4)-66.914*(x**3)+70.684*(x**2)-38.168*x+8.112+0.0125*(log(x)))**2 
end function f

program golden
    implicit none
    real*8 b,c,d,e,l,m,n,g,h,f,y,min,mm
    integer i,j,k,x
    g=0.381966
    h=1d-5
    x=0
    min=1d+7
    do i=0,10000000
            b=i*h
            c=b+h
            d=b+(2*h)
            if (((f(c))<f(b)) .and. ((f(c))<f(d))) then
                do k=1,2000
                    if ((d-c)>(c-b)) then
                        e=d-(g*(d-b))
                        if (f(e)<f(c)) then
                            b=c
                            c=e
                        else
                            d=e
                        endif
                    else
                    e=b+(g*(d-b))
                        if (f(e)<f(c)) then
                            d=c
                            c=e
                        else
                            b=e
                        endif
                    endif
                    if (abs(((b-d)/d))<=1d-6) exit
                enddo
            print *,c,f(c)
            if (min>f(c)) then
                min=f(c)
                mm=c
            endif
            endif
        enddo
    print *,"Minimum value of function is ", min," at x = ", mm
    
end program golden