PROGRAM Q
    REAL::term,n,l,sum,x,o
    integer k
    n=1
    x=1
    l=1
    do 
        if (n==50) exit
        term=(log(x))/(x**(0.01))
        print *,term,x
        x=x*2
        n=n+1
    enddo
    
    n=1
    x=1
    do 
        if (n==50) exit
        term=(exp(x)-1)/(x)
        print *,term,x
        x=x/2
        n=n+1
    enddo
    
    n=1
    x=1
    do 
        if (n==50) exit
        term=(cos(x)+cosh(x)-2)/(x**4)
        print *,term,x
        x=x/2
        n=n+1
    enddo
    
    n=1
    x=1
    do 
        if (n==50) exit
        term=((1+(1/x))**x)-exp(l)
        print *,term,x
        x=x*2
        n=n+1
    enddo
    
    n=1
    x=1
    do 
        if (n==50) exit
        term=((cosh(x))/(sinh(x)))
        print *,term,x
        x=x*2
        n=n+1
    enddo
    
    n=1
    x=1
    do 
        if (n==50) exit
        term=(x*(sqrt(1+x*x)))-(x*x)
        print *,term,x
        x=x*2
        n=n+1
    enddo
    
    n=1
    x=1
    do 
        if (n==50) exit
        term=(x**(0.01))*log(x)
        print *,term,x
        x=x/2
        n=n+1
    enddo
    
    n=1
    k=1
    x=1
    do
        if (n==50) exit
        sum=0
        do i=1,k
            o=i
            term=1/o
            sum=sum+term
            l=k
        enddo
        print *,sum-log(l),k
        k=k*2
        n=n+1
    enddo
    
end program q