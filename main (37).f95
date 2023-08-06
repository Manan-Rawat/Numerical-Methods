real*8 function f(e,x)
    implicit none
    real*8 e,x
    f=(x-e*sin(x))**(2)
end function f

real*8 function d(e,x)
    implicit none
    real*8 e,h,x,f
    integer i
    h=1
    do i=1,20
        d=(f(x+h,e)-f(x,e))/h
        h=h/2
    enddo
end function d    
    
program der
    implicit none
    real*8 e1,e2,e3,e,g,g1,ll,ul,h,d,a,b,mid,x
    real*8,dimension(3)::Y
    integer i,j,k
    g=(sqrt(5.0)+1)/2
    g=1/g
    g1=1-g
    ll-30
    h=0.05
    Y=(/10.0,20.0,20.3959/)
    do j=1,3
        ll=-30
        h=0.05
        ul=30
        a=ll
        b=ul
        e=Y(j)
         do i=1,100
                mid=(a+b)/2
                if (d(mid,e)*d(b,e)>0) then
                    b=mid
                else
                    a=mid
                endif
                if (abs(b-a)<1e-7) exit
        enddo            
        print *,mid,d(mid,e),e
    enddo
end program der        