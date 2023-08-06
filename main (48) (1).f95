program start 
real*8:: a, b, c, x1, x2, x3, x4, t, f  
integer:: l , m, n 
t = 0.381966
h = 1d-05
do i = 0 , 2000000
    x1 = i*h
    x2 = x1 + h 
    x3 = x1 + 2d0*h
    if(f(x2).lt.f(x1) .and. f(x2).lt.f(x3)) then 
    do j = 1 , 1500
        if ((x3 - x2).gt.(x2 - x1)) then 
            x4 = x3 - t*(x3 - x1)
                if (f(x4).lt.f(x2)) then 
                    x1 = x2 
                    x2 = x4 
                else 
                    x3 = x4 
                endif 
        else
            x4 = x1 + t*(x3 - x1)
                if (f(x4).lt.f(x2)) then 
                    x3 = x2 
                    x2 = x4 
                    else 
                    x1 = x4 
                endif 
        endif
    if ( abs((x1-x3)/x3).le.1d-06) exit 
    !print*, x1 , x2, x3 
    enddo
    print*, x2, f(x2)
    endif  
enddo

end 
!function f(x)
!    real*8:: f, x, a 
!    a = 20.3959 
 !   f = (x - a*sin(x))*(x - a*sin(x))
!end 

!function f(x)
!    real*8:: f, x 
!   f = x*sin(x)
!end 

!function f(x) 
!   real*8:: f, x 
!    f = ((x**6) - 36*(x**5) + 450*(x**4) - 2400*(x**3) + 5400*(x**2) - 4520*x + 720)
!end 

function f(x) 
    real*8:: f, x 
    f = ((x**6) - 9.2d0*(x**5) + 34.45d0*(x**4) - 66.914d0*(x**3)+70.684d0*(x**2) - 38.168d0*x+8.112d0+ 0.0125d0*(log(x)))**2 
!if(x == 0) f = 1d100
!print*, x , f 
end 
    



