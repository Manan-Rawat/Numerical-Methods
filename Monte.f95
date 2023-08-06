program monte
    implicit none
    integer i,j,n
    real sum,k,a,b,mean,r,sum1,var,m,r1,x1,D,diff
    sum=0
    sum1=0
    diff=0
    mean=0
    n=1000000
    m=n
    do i=1,n
        call random_number(r)
        sum1=sum1+(exp(r))**(2)
        sum=sum+exp(r)
    enddo
        sum=sum/m
        sum1=sum1/m
        var=sum1-sum**(2)
        diff=sum-exp(1.0)+1
    print *,var,sqrt(var/n)*sqrt(var),diff
    sum=0
    sum1=0
    diff=0
    mean=0
    n=1000000
    m=n
    do i=1,n
        call random_number(r)
        sum1=sum1+(exp(r)-1-r)**(2)
        sum=sum+(exp(r)-1-r)
    enddo
        sum=sum/m
        sum1=sum1/m
        var=sum1-sum**(2)
    print *,var,sqrt(var/n)
    
    !!px=2/3(1+x)
    
    sum=0
    sum1=0
    diff=0
    mean=0
    n=1000000
    m=n
    do i=1,n
        call random_number(r)
        D=4+12*r
        x1=(-2+sqrt(D))/(2)
        r1=(x1)
        sum1=sum1+(((exp(r1)*3/(2*(1+r1)))**(2)))
        sum=sum+(exp(r1)*3/(2*(1+r1)))
    enddo
        sum=sum/m
        sum1=sum1/m
        var=sum1-sum**(2)
    print *,var,sqrt(var/n),sum
    
end program monte