program monte
    implicit none
    integer i,j,n
    real sum,k,a,b,mean,r,diff,diff1,sum1,var,m
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
    print *,var,sqrt(var/n),diff
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
        sum1=sum1+(((exp(r)*3/(2*(1+r)))**(2))*(2*(1+r)/3))
        sum=sum+(exp(r))
    enddo
        sum=sum/m
        sum1=sum1/m
        var=sum1-sum**(2)
    print *,var,sqrt(var/n)
    
    
end program monte