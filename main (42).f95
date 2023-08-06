real*16 function prod(t)
    IMPLICIT NONE
    real*16,intent(in)::t
    real*16,dimension(20,20)::A,NA,B
    real*16,dimension(20)::X,B1
    integer n,i,j,k,l
    real*16 d,c,sum,temp,max
    A=0
    B=0
    NA=0
    B1=0
    X=0
    n=4
        prod=1
        do i=1,n
            c=i
            do j=1,n
                d=j
                A(i,j)=1.0/(c+d-1)
            enddo
        enddo
        do i=1,n
            A(i,i)=A(i,i)-t
        enddo
        do i=1,n
            sum=0
            do j=1,n
                sum=sum+A(i,j)
            enddo
            B1(i)=sum
        enddo
        do i=1,n
            B(i,i)=1
        enddo
        do k=1,n-1
            j = maxloc(abs(A(k:n,k)),dim=1)+k-1 
            if (j/=k) then
                do l=k,n
                    temp=A(k,l)
                    A(k,l)=A(j,l)
                    A(j,l)=temp
                enddo
                do l=1,n
                    temp=B(k,l)
                    B(k,l)=B(j,l)
                    B(j,l)=temp
                enddo
            endif
            do i=1,n-k
                c=(A(k+i,k))/(A(k,k))
                A(k+i,k+1:n) = A(k+i,k+1:n)-(c*(A(k,k+1:n)))
                B(k+i,1:n) = B(k+i,1:n)-(c*(B(k,1:n)))
            enddo
        enddo
            do i=1,n
                prod=prod*A(i,i)
            enddo
end function prod   

program quadrature
    implicit none
    integer i,j,k,l,p,d
    real*16 m,h,a,sum,n,t,integ,fin,ul,ll,x,ln,b,mid,f,prod
    l=50
    ln=0
    a=1.5
    b=2
    mid=(a+b)/2
    do k=1,100
        if (prod(mid)>0) then
            a=a
            b=mid
            mid=(a+b)/2
        else
            b=b
            a=mid
            mid=(a+b)/2
        endif
        if (abs((prod(mid)))<1d-6) exit
        print *,mid,prod(mid),a,b
    enddo
END program quadrature    
                
