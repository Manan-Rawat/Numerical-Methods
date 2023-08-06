real*8 function Df(k)
    real*8 k 
    Df=exp(-2*(abs((-16)+(k/2))))
end function Df

program DFT
    implicit none
    real*8 a,b,d,k,Df,pi
    integer i,j
    complex*8::C(0:63),im
    pi=4*atan(1.0)
    do j=0,63
        do i=0,63
            k=i
            C(j)=C(j)+Df(k)*cos(2*pi*j*i/64)+im*(sin(2*pi*j*i/64))
        enddo
    enddo
    do j=32,63
        print *,j-64,abs(c(j))
    enddo
    do j=0,31
        print *,j,abs(c(j))
    enddo
end program dft