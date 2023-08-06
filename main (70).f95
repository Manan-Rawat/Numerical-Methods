real*8 function R(k,a)
    implicit none
    real*8 pi,n
    real*8,intent(in)::k,a
    n=128
    pi=4*atan(1.0)
    R=(sin((2.0)*pi*a*k))/n
end function R
    
    
    
program DFT
    real*8 a,b,d,e,x,f,g,h,R,n,k
    integer i,j,o
    real*8,dimension(3)::L
    complex*16 :: C(128),im
    n=128
    L=(/60.0,60.5,120.0/)
    do i=1,3
        a=L(i)
        do j=1,128
            do o=1,128
                k=o
                C(j)=C(j)+(R(k,a))*(cos((2*pi*j*k)/n))+im*(sin((pi*2*j*k)/n))
            enddo
        enddo
        do j=65,128
            print *,j-n-1,abs(C(j)),a
        enddo
        do j=1,64
            print *,j-1,abs(C(j)),a
        enddo
    enddo
end program DFT