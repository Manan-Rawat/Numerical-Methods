PROGRAM interpolation
real*8,dimension(10,10)::six
real*8,dimension(10)::dif,inter,temp
real*8,dimension(11)::g
real*8,dimension(5)::X
real*8,dimension(4)::Y
real*8::n,a,b
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
six(i,1)=sin(1.560+(0.001*i))
dif(i)=1.560+(0.001*i)
enddo
do i=1,10
six(i,1)=(1.560+(0.001*i))
dif(i)=sin(1.560+(0.001*i))
enddo
do i=2,10
do j=i,10
six(j,i)=(six(j,i-1)-six(j-1,i-1))/(dif(j)-dif(j-i+1))
enddo
enddo
do i=1,10
print *,(six(i,j),j=1, 10)
enddo
G=0
do j=1,4
 do i=1,10
 G(i+1)=six(i,i)
 enddo
 n=Y(j)
 do i=10,1,-1
 G(i)=(G(i+1)*(n-dif(i)))+six(i,i)
 enddo
 print *,"FOR sin(X) = ",n,"THE INTERPOLATED VALUE OF X IS ",G(1)
 enddo
 do i=1,10
six(i,1)=(1.564+(0.001*i))
dif(i)=sin(1.564+(0.001*i))
enddo
do i=2,10
do j=i,10
six(j,i)=(six(j,i-1)-six(j-1,i-1))/(dif(j)-dif(j-i+1))
enddo
enddo
do i=1,10
print *,(six(i,j),j=1, 10)
enddo
G=0
do j=1,4
 do i=1,10
 G(i+1)=six(i,i)
 enddo
 n=Y(j)
 do i=10,1,-1
 G(i)=(G(i+1)*(n-dif(i)))+six(i,i)
 enddo
 print *,"FOR sin(X) = ",n,"THE INTERPOLATED VALUE OF X IS ",G(1)
 enddo
 
 
end program interpolation
