PROGRAM Q
    REAL::term,n,sum,sum1,x,k
    REAL,DIMENSION(10)::list
    list=(/-0.5,0.5,-1.0,1.0,-5.0,5.0,-20.0,20.0,-50.0,50.0/)
    do j=1,10
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
        print *,sum,exp(x),"error =",ABS(SUM-EXP(X))
    enddo
END PROGRAM q