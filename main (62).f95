program start 
real*8:: a, b, s, s1, c, f, pi, g1  
integer:: l, m, n 
complex*16:: G(0:200), im
g1(k) = sin(2d0*pi*f*k/n)
pi = 4d0*atan(1d0)
n = 128d0 
f = 60d0
G = 0d0 
im = (0.0, 1d0)
do j = 0, n-1
    do k = 0, n-1 
       G(j) = G(j) + g1(k)*(cos(2d0*pi*j*k/n) + im*(sin(2d0*pi*j*k/n)))
       ! G(j) = G(j) + g1(k)*(exp(2d0*pi*im*j*k/n))
    enddo 
    
enddo 
 do j = n/2, n- 1 
    print*, j- n , abs(g(j))
    enddo 
    
    
do j = 0, n/2- 1
    print*, j , abs(g(j))
enddo
end 




