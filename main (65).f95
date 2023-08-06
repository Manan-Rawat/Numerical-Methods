program start 
real*8:: b, s, s1, c, f, pi, g1  
integer:: l, m, n, a
complex*16:: G(0:200), im, g2
g1(k) = exp(-2d0*abs((2d0*a*k/n) - a))   !"tk = 2*k*a/n - a" 
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
    print*, j- n , abs(g(j))
    enddo 
    
    
do j = 0, n/2- 1
    print*, j , abs(g(j))
enddo

print*, "inverse fourier transform from here on"
do j = 0, 512
    g2 = 0 
    t = -a + 2d0*a*j/512
    do k = 0, n/2-1
        g2 = g2 + (1d0/n)*G(k)*(exp(-2d0*pi*im*(k)*(t + a)/(2*a))) !"k/n = (t + a)/2*a" 
    enddo 
    do k = n/2 , n-1
        g2 = g2 + (1d0/n)*G(k)*(exp(-2d0*pi*im*(k - n)*(t + a)/(2*a))) !"k/n = (t + a)/2*a" 
    enddo 
  print*, t, g2 
enddo 



end program 