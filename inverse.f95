program inverse
	implicit none
	real,dimension(20,20)::m5,m10
	real,dimension(20)::temp,x
	integer k,a,b,c,h,j,i,t
	real::m,n,d,e,f,sum
	temp=0
	m5=0
	do k=5,15,5	
		m10=0
		x=0
		do i=1,k
			m=i
			m10(i,i)=1
			do j=1,k
				n=j
				m5(i,j)=1.0/(m+n-1)
			enddo
		enddo
		do i=1,k
			do j=1,k
				x(i)=x(i)+m5(i,j)
			enddo
		enddo
		do a=1,k-1
			d=abs(m5(a,a))
			h=a
			do b=a+1,k 
				if (abs(m5(a,b))>d) then
				d=abs(m5(a,b))
				h=b
				endif
			enddo
			if (h/=a) then
				do i=a,k
					temp(i)=m5(a,i)
					m5(a,i)=m5(h,i)
					m5(h,i)=temp(i)
				enddo
				do i=1,k
					temp(i)=m10(k,i)
					m10(k,i)=m10(h,i)
					m10(h,i)=temp(i)
				enddo
			endif
		do j=a+1,k
			m5(j,a)=m5(j,a)/m5(a,a)
			do i=a+1,k
				m5(j,i)=m5(j,i)-((m5(j,a))*(m5(a,i)))
			enddo
			do i=1,k
				m10(j,i)=m10(j,i)-((m5(j,a))*(m10(a,i)))
			enddo
		enddo
		enddo
		x=0
		do t=k,1,-1
			sum=0
			do j=a+1,k
				sum=sum+(m5(a,t)*x(t))
			enddo
			x(t)=(m10(t,t)-sum)/m5(t,t)
			do j=1,k
				m10(t,t)=x(t)
			enddo
		enddo
		do i=1,k
			print *,(m10(i,j),j =1, k)
		enddo
		
	enddo
end program inverse
