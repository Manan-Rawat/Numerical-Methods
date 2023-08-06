program start
real*8:: b, s, s1, c, f, pi, g1  
integer:: l, m, n, a
complex*16:: G(0:200), im
g1(k) = exp(-2d0*abs((2d0*a*k/n) - a))
pi = 4d0*atan(1d0)
n = 64d0 
a = 16d0
G = 0d0 
im = (0.0, 1d0)
do j = 0, n-1
    do k = 0, n-1 
        G(j) = G(j) + g1(k)*(exp(2d0*pi*im*j*k/n))
    enddo 
    
enddo 
 do j = n/2, n- 1 
    print*, j- n , abs(G(j))
    enddo 
    
    
do j = 0, n/2- 1
    print*, j , abs(G(j))
enddo



end program 