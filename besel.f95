real*8 function B(x)
    implicit none
    real*8,intent(in)::x
    real*8 i,f,p
    integer k
    i=0
    f=1
    B=0
    do k=1,20
        p=k
        B=B+((-1)**(p))*((x/2)**(2*p))/(f*f)
        i=i+1
        f=f*(i)
    enddo
end function B

program roots
    implicit none
    real*8 a,b,c,x,h,ll,ul,mid,f,d
    integer k,p
    ll=0
    h=0.1
    ul=ll+h
    c=0
    do 
        if ((B(ul)*B(ll))<0) then     
            a=ll
            d=ul
            mid=(a+d)/2
            do p=1,100
                mid=(a+d)/2
                if (B(mid)*B(d)>0) then
                    d=mid
                else
                    a=mid
                endif
                if (abs(d-a)<1e-7) exit
            enddo
            print *,mid,c+1,B(mid)
            c=c+1
        endif
    ll=ul
    ul=ul+h
    if (c==3) exit
    enddo
END program