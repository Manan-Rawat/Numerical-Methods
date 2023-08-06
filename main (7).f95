
PROGRAM q1
    IMPLICIT NONE
    REAL*16,dimension(4,4)::table
    REAL*16,dimension(4)::sol
    REAL*16,dimension(4)::sol1
    real*16::a,b,c,d,sum
    real*16,dimension(50)::f
    integer i,j,k
    table=0
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
    do i=1,48
        f(i+2)=f(i+1)+f(i)
    enddo
    do i=1,50
        print *,f(i),i
    enddo
    
end program q1
        
