PROGRAM interpolation
	real,dimension(10,10)::six
	real,dimension(10)::dif,inter,temp
	real,dimension(11)::g
	real,dimension(5)::X
	real,dimension(4)::Y
	real::n,a,b
	integer j,i
	Y=(/0.999995,0.99995, 0.9999, 0.9995/)
	X=(/1.5716, 1.5736, 1.5635, 1.5605, 1.5585/)
	six=0
	do i=1,10
		six(i,1)=sin(1.560+(0.001*i))
		dif(i)=1.560+(0.001*i)
	enddo
	do i=2,10
		do j=i,10
			six(j,i)=(six(j,i-1)-six(j-1,i-1))/(dif(j)-dif(j-i+1))
		enddo
	enddo	
	do i=1,10
		print *,(six(i,j),j=1, 10)
	enddo
	do j=1,5
	    do i=1,10
            G(i+1)=six(i,i)
        enddo
	    n=X(j)
        do i=10,1,-1
            G(i)=(G(i+1)*(n-dif(i)))+six(i,i)
        enddo
    print *,"FOR X = ",n,"THE INTERPOLATED VALUE OF SIN IS ",G(1)
    enddo
	do i=1,10
		temp(i)=six(i,1)
		six(i,1)=six(i,2)
		six(i,2)=temp(i)
	enddo
	do i=2,10
		do j=i,10
			six(j,i)=(six(j,i-1)-six(j-1,i-1))/(dif(j)-dif(j-i+1))
		enddo
	enddo
	do j=1,4
	    do i=1,10
            G(i+1)=six(i,i)
        enddo
	    n=Y(j)
        do i=1,10
            G(i+1)=(G(i)-six(i,i))/(n-dif(i))
        enddo
    print *,"FOR sin(X) = ",n," X = ",G(10)
    enddo
	Y=(/0.999995,0.99995, 0.9999, 0.9995/)
	X=(/1.5716, 1.5736, 1.5635, 1.5605, 1.5585/)
	six=0
	do i=1,10
		six(i,1)=sin(1.564+(0.001*i))
		dif(i)=1.564+(0.001*i)
	enddo
	do i=2,10
		do j=i,10
			six(j,i)=(six(j,i-1)-six(j-1,i-1))/(dif(j)-dif(j-i+1))
		enddo
	enddo	
	do j=1,5
	    do i=1,10
            G(i+1)=six(i,i)
        enddo
	    n=X(j)
        do i=10,1,-1
            G(i)=(G(i+1)*(n-dif(i)))+six(i,i)
        enddo
    print *,"FOR X = ",n,"THE INTERPOLATED VALUE OF SIN IS ",G(1)
    enddo
	do i=1,10
		temp(i)=six(i,1)
		six(i,1)=six(i,2)
		six(i,2)=temp(i)
	enddo
	do i=2,10
		do j=i,10
			six(j,i)=(six(j,i-1)-six(j-1,i-1))/(dif(j)-dif(j-i+1))
		enddo
	enddo
    print *,six
	do j=1,4
	    do i=1,10
            G(i+1)=six(i,i)
        enddo
	    n=Y(j)
        do i=1,10
            G(i+1)=(G(i)-six(i,i))/(n-dif(i))
        enddo
    print *,"FOR sin(X) = ",n," X = ",G(10)
    enddo
	
end program interpolation 

