real*8 function f(e,x)
    implicit none
    f=(x-e*sin(x))**(2)
end function f

program der
    implicit none
    real*8 e1,e2,e3,e,g,g1
    real*8,dimension(3)::Y
    g=(sqrt(5.0)+1)/2
    g=1/g
    g1=1-g
    integer j,p,k
    Y=(/10,20,20.3959/)
    do j=1,3
        e=Y(j)
        e1=0
        e3=1
        do 
            
        
        
                
                
            
    
        