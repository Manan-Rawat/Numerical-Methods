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
    real*8 e1,e2,e3,e,g,g1,ll,ul,h,d,a,b,mid,x,f
    real*8,dimension(3)::Y
    integer i,j,k
    Y=(/10.0,20.0,20.3959/)
    g=(sqrt(5.0)+1)/2
    ll=-30
    ul=30
    h=0.01
    g=1/g
    g1=1-g
    do j=1,3
        e=Y(j)
        a=ll
        b=ul
        do 
            mid=a+h
            if ((f(mid,e))<min((f(a,e)),(f(b,e)))) then
                if (abs(mid-a)>abs(b-mid)) then
                        a=a
                        b=mid
                        mid=(a+b)/2
                else
                        a=mid
                        b=b
                        mid=(a+b)/2
                endif
            else
                mid=mid+h
            endif
            if (d(mid,e)<1e-3) exit
        enddo
        print *,mid,f(mid,e),d(mid,e),e
    enddo
end program der        
        