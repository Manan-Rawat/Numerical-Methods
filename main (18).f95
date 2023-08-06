PROGRAM Q5
    real*8,dimension(4,4)::six
    real*8,dimension(4)::inter
    real*8,dimension(10,10)::six1
    real*8,dimension(10)::inter1
    real*8,dimension(20,20)::six2
    real*8,dimension(20)::inter2
    real m,x,y,z
    integer i,j,n
    m=0
    x=0
    y=0
    z=0
    do n=2,4,2
        do i=1,n
            inter(i)=cos(((2*i)-1.0)*3.14159/(2*n))
            six(i,1)=sin(inter(i))
        enddo
        do j=2,n
            do i=j,n
                six(i,j)=(six(i,j-1)-six(i-1,j-1))/(inter(i)-inter(i-j+1))
            enddo
        enddo
        m=0
        do i=0,200
            x=(-1)+0.01*i
            z=six(n,n)
            do j=n-1,1,-1
                z=z*(x-inter(j))+six(j,j)
            enddo
            m=max(m,abs(g-sin(x)))
        enddo
        print *,n,m
    enddo
    m=0
    x=0
    y=0
    z=0
    do n=2,10,2
        do i=1,n
            inter1(i)=cos(((2*i)-1.0)*3.14159/(2*n))
            six1(i,1)=sin(inter1(i))
        enddo
        do j=2,n
            do i=j,n
                six1(i,j)=(six1(i,j-1)-six1(i-1,j-1))/(inter1(i)-inter1(i-j+1))
            enddo
        enddo
        m=0
        do i=0,200
            x=(-1)+0.01*i
            z=six1(n,n)
            do j=n-1,1,-1
                z=z*(x-inter1(j))+six1(j,j)
            enddo
            m=max(m,abs(g-sin(x)))
        enddo
        print *,n,m
    enddo
    m=0
    x=0
    y=0
    z=0
    do n=2,20,2
        do i=1,n
            inter2(i)=cos(((2*i)-1.0)*3.14159/(2*n))
            six2(i,1)=sin(inter2(i))
        enddo
        do j=2,n
            do i=j,n
                six2(i,j)=(six2(i,j-1)-six2(i-1,j-1))/(inter2(i)-inter2(i-j+1))
            enddo
        enddo
        m=0
        do i=0,200
            x=(-1)+((0.01)*i)
            z=six2(n,n)
            do j=n-1,1,-1
                z=z*(x-inter2(j))+six2(j,j)
            enddo
            m=max(m,abs(z-sin(x)))
        enddo
        print *,n,m
    enddo
    m=0
    x=0
    y=0
    z=0
    do n=2,4,2
        do i=1,n
            inter(i)=cos(((2*i)-1.0)*3.14159/(2*n))
            six(i,1)=exp(inter(i))
        enddo
        do j=2,n
            do i=j,n
                six(i,j)=(six(i,j-1)-six(i-1,j-1))/(inter(i)-inter(i-j+1))
            enddo
        enddo
        m=0
        do i=0,200
            x=(-1)+0.01*i
            z=six(n,n)
            do j=n-1,1,-1
                z=z*(x-inter(j))+six(j,j)
            enddo
            m=max(m,abs(g-sin(x)))
        enddo
        print *,n,m
    enddo
    m=0
    x=0
    y=0
    z=0
    do n=2,10,2
        do i=1,n
            inter1(i)=cos(((2*i)-1.0)*3.14159/(2*n))
            six1(i,1)=exp(inter1(i))
        enddo
        do j=2,n
            do i=j,n
                six1(i,j)=(six1(i,j-1)-six1(i-1,j-1))/(inter1(i)-inter1(i-j+1))
            enddo
        enddo
        m=0
        do i=0,200
            x=(-1)+0.01*i
            z=six1(n,n)
            do j=n-1,1,-1
                z=z*(x-inter1(j))+six1(j,j)
            enddo
            m=max(m,abs(g-sin(x)))
        enddo
        print *,n,m
    enddo
    m=0
    x=0
    y=0
    z=0
    do n=2,20,2
        do i=1,n
            inter2(i)=cos(((2*i)-1.0)*3.14159/(2*n))
            six2(i,1)=exp(inter2(i))
        enddo
        do j=2,n
            do i=j,n
                six2(i,j)=(six2(i,j-1)-six2(i-1,j-1))/(inter2(i)-inter2(i-j+1))
            enddo
        enddo
        m=0
        do i=0,200
            x=(-1)+((0.01)*i)
            z=six2(n,n)
            do j=n-1,1,-1
                z=z*(x-inter2(j))+six2(j,j)
            enddo
            m=max(m,abs(z-sin(x)))
        enddo
        print *,n,m
    enddo
end program q5