PROGRAM interpolation
	real,dimension(8,8)::six
	real,dimension(8)::dif,inter,temp
	real,dimension(9)::f
	real,dimension(4)::Y,X
	real::n,a,b
	integer j,i
	six =0
	G=0
	X=(/1000,2000,25000,42195/)
	dif=(/100,200,400,800,1500,5000,10000,30000/)
	six(1,1)=9.58
	six(2,1)=19.19
	six(3,1)=43.03
	six(4,1)=(1*60)+(40.91)
	six(5,1)=(3*60)+(26.00)
	six(6,1)=(12*60)+(35.36)
	six(7,1)=(26*60)+(11.00)
	six(8,1)=(1*3600)+(26*60)+(47.40)
	do i=2,8
		do j=i,8
			six(j,i)=(six(j,i-1)-six(j-1,i-1))/(dif(j)-dif(j-i+1))
		enddo
	enddo
	do i=1,8
	    print *,(six(i,j), j=1,8)
	enddo
	print *,"enter value at which time is req."
	read *,n
	do i=1,8
        f(i+1)=six(i,i)
        enddo
        do i=8,1,-1
        f(i)=(f(i+1)*(n-dif(i)))+six(i,i)
    enddo
	    print *,"FOR X = ",n,"THE INTERPOLATED VALUE IS ",f(1)
end program interpolation