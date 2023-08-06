real*8 function v1f(x)
    implicit none
    real*8 r1f,a
    real*8,intent(in)::x
    common a
    r1f=a*sqrt(((3*x*x)+(3*cos(x))-8)/(2*x))
    v1f=(-2*x*x)-(3*x*(r1f))+(4*(sin(r1f)))+6
end function v1f

program roots
    implicit none
    real*8 a,b,c,x,h,ll,ul,mid,f,d,v1f
    common d
    integer k,p,u
    d=1
    do u=1,2
       d=d*(-1)
    ll=-4
    h=1d-3
    ul=ll+h
    c=0
    do 
        if ((v1f(ul)*v1f(ll))<0) then     
            a=ll
            b=ul
            mid=(a+b)/2
            do p=1,100
                mid=(a+b)/2
                if (v1f(mid)*v1f(b)>0) then
                    b=mid
                else
                    a=mid
                endif
                if (abs(b-a)<1e-7) exit
            enddo
            print *,mid,d*sqrt(((3*mid*mid)+(3*cos(mid))-8)/(2*mid)),d
            c=c+1
        endif
    ll=ul
    ul=ul+h
        if (ul>4) exit
    enddo
    enddo
END program
