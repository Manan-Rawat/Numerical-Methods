PROGRAM q1
    implicit none
    REAL*16,dimension(4,4)::table
    REAL*16,dimension(4)::sol
    REAL*16,dimension(4)::sol1
    real*16::a,b,c,d,sum,s1,s2
    real*16,dimension(60)::f
    integer,dimension(4)::list
    real*16,dimension(2,2)::ftable
    real*16,dimension(2)::fsol
    integer i,j,k,n,l
    table=0
    list(1)=5
    list(2)=10
    list(3)=20
    list(4)=50
    table(1,1)=0.000007143
    table(2,1)=0.6262
    table(3,1)=0.4923
    table(4,1)=0.8017
    table(2,2)=0.000007355
    table(3,2)=0.2123
    table(4,2)=0.6123
    table(3,3)=0.000002534
    table(4,3)=0.7165
    table(4,4)=0.000004133
    sol(1)=0.000009245
    sol(2)=0.3763
    sol(3)=0.6087
    sol(4)=0.4306
    do i=1,4
        sum=sol(i)
        j=1
        do
            k=i
            if(i==j) exit
            sum=sum-(sol1(k-1)*table(k,j))
            j=j+1
        enddo
        sol1(i)=sum/(table(i,i))
    enddo
    do i=1,4
        print *,sol1(i)
    enddo
    do i=1,4
        sum=0
        do j=1,4
            sum=sum+table(i,j)
        enddo
        sol(i)=sum
    enddo
    do i=1,4
        sum=sol(i)
        j=1
        do
            k=i
            if(i==j) exit
            sum=sum-(sol1(k-1)*table(k,j))
            j=j+1
        enddo
        sol1(i)=sum/(table(i,i))
    enddo
    do i=1,4
        print *,sol1(i)
    enddo
    f(1)=1
    f(2)=1
    do i=1,58
        f(i+2)=f(i+1)+f(i)
    enddo
    do i=1,4
        n=list(i)
        ftable(1,1)=f(n)
        ftable(2,1)=f(n+1)
        ftable(1,2)=f(n+1)
        ftable(2,2)=f(n+2)
        fsol(1)=f(n+2)
        fsol(2)=f(n+3)
        s1=((ftable(2,1)*fsol(2))-(fsol(1)*ftable(2,2)))/((ftable(2,1)*ftable(1,2))-(ftable(1,1)*ftable(2,2)))
        s2=((ftable(1,1)*fsol(2))-(fsol(1)*table(1,2)))/((ftable(2,1)*ftable(1,2))-(ftable(1,1)*ftable(2,2)))
        print *,"solution for table n =",list(i)
        do k=1,2
            print *,(ftable(k,l),l=1, 2)
        enddo
        print *,"x1 = ", s1, "x2 = ", s2
    enddo
end program q1