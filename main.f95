PROGRAM Q
    REAL::term,n,sum,sum1,x,k
    REAL,DIMENSION(4)::list
    list=(/1,2,3,4/)
    do j=1,4
        n=1
        k=1
        sum=1
        sum1=0
        term=1
        x=list(j)
        do
            if (sum==sum1) exit
            sum1=sum
            term=(x/n)*(term)
            sum=sum+term
            n=n+1
        enddo
        print *,sum,exp(x)
    enddo
 end program Q