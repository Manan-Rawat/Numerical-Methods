PROGRAM Q2
    IMPLICIT NONE
    real,dimension(20,20)::A,NA,B
    real,dimension(20)::X,B1
    integer n,i,j,k,l
    real d,c,sum,temp,max,prod
    A=0
    B=0
    NA=0
    B1=0
    X=0
    do n=5,15,5
        prod=1
        do i=1,n
            c=i
            do j=1,n
                d=j
                A(i,j)=1.0/(c+d-1)
            enddo
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
            do j=1,n
                do k=n,1,-1
                    sum=0
                    do i=k+1,n
                        sum=sum+(A(k,i)*(B(i,j)))
                    enddo
                    b(k,j)=((b(k,j))-sum)/(a(k,k))
                enddo
            enddo
            do i=1,n
                prod=prod*a(i,i)
            enddo
            print *," A(i,j) = 1 / (i+j-1)"
            print *," n = ", n
            print *," INVERSE MATRIX "
            print *," Value of determinant ", prod
        do i=1,n
            print *, (b(i,j), j=1,n)
        enddo
        do i=1,n
            sum=0
            do j=1,n
                sum=sum+(b(i,j))*(B1(j))
            enddo
            X(i)=sum
        enddo
        print *,"X"
        print *,(X(i), i=1,n)
    enddo
    A=0
    B=0
    NA=0
    B1=0
    X=0
    do n=5,15,5
        prod =1
        do i=1,n
            c=i
            do j=1,n
                d=j
                A(i,j)=(sqrt(2.0/(n+1)))*(sin(i*j*3.14159/(n+1)))
            enddo
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
            do j=1,n
                do k=n,1,-1
                    sum=0
                    do i=k+1,n
                        sum=sum+(A(k,i)*(B(i,j)))
                    enddo
                    b(k,j)=((b(k,j))-sum)/(a(k,k))
                enddo
            enddo
            print *," A(i,j) = (2/n+1)**(0.5) * sin(ij/n+1)"
            do i=1,n
                prod=prod*a(i,i)
            enddo
            print *," determinant = ",prod
            print *," n =",n
            print *," INVERSE MATRIX "
        do i=1,n
            print *, (b(i,j), j=1,n)
        enddo
        do i=1,n
            sum=0
            do j=1,n
                sum=sum+(b(i,j))*(B1(j))
            enddo
            X(i)=sum
        enddo
        print *,"X"
        print *,(X(i), i=1,n)
    enddo
end program q2                
                
