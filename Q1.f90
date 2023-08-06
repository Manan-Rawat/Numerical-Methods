PROGRAM difference
	real*8::h,diff31,diff32,diff51,diff52,t,e31,e32,e51,e52,miner31,miner32,miner52,miner51,h31,h32,h51,h52
	real*8::d31(0:20,0:20),d32(0:20,0:20),d51(0:20,0:20),d52(0:20,0:20)
	miner31=100000
	miner32=100000
	miner51=100000
	miner52=100000
	h31=2
	h32=2
	h51=2
	h52=2
	h=1
	t=0
	d31=0
	d32=0
	d51=0
	d52=0
	do n=0,20
		diff31=(exp(t+h)-exp(t-h))/(2*h)
		d31(n,0)=diff31
		e31=abs(diff31-1)
		diff32=((exp(t+h))-(2*(exp(t)))+exp(t-h))/(h*h)
		d32(n,0)=diff32
		e32=abs(diff32-1)
		diff51=((exp(t-2*h))-(8*(exp(t-h)))+(8*(exp(t+h)))-(exp(t+2*h)))/(12*h)
		d51(n,0)=diff51
		e51=abs(diff51-1)
		diff52=(-(exp(t-2*h))+(16*exp(t-h))-(30*(exp(t)))+(16*(exp(t+h)))-exp(t+(2*h)))/(12*h*h)
		d52(n,0)=diff52
		e52=abs(diff52-1)
		print *,diff31,diff32,diff51,diff52
		if (e31<miner31) then
			miner31=e31
			h31=h
		endif
		if (e32<miner32) then
			miner32=e32
			h32=h
		endif
		if (e51<miner51) then
			miner51=e51
			h51=h
		endif
		if (e52<miner52) then
			miner52=e52
			h52=h
		endif
		h=h/2
	enddo
	PRINT *,"OPTIMAL h for 1st derivative 3 point is",h31, "with error",miner31
	PRINT *,"OPTIMAL h for 2nd derivative 3 point is",h32, "with error",miner32
	PRINT *,"OPTIMAL h for 1st derivative 5 point is",h51, "with error",miner51
	PRINT *,"OPTIMAL h for 2nd derivative 5 point is",h52, "with error",miner52
	do i=1,20
		do j=1,i-1
			d31(i,j)=d31(i,j-1)+(d31(i,j-1)-d31(i-1,j-1))/((4**(j))-1)
		enddo
	enddo
	do i=0,5
		print *,(d31(i,j), j=0,5)
	enddo
	do i=1,20
		do j=1,i-1
			d32(i,j)=d32(i,j-1)+(d32(i,j-1)-d32(i-1,j-1))/((4**(j))-1)
		enddo
	enddo
	do i=0,5
		print *,(d32(i,j), j=0,5)
	enddo
	do i=1,20
		do j=1,i-1
			d51(i,j)=d51(i,j-1)+(d51(i,j-1)-d51(i-1,j-1))/((4**(j))-1)
		enddo
	enddo
	do i=0,5
		print *,(d51(i,j), j=0,5)
	enddo
	do i=1,20
		do j=1,i-1
			d52(i,j)=d52(i,j-1)+(d52(i,j-1)-d52(i-1,j-1))/((4**(j))-1)
		enddo
	enddo
	do i=0,5
		print *,(d52(i,j), j=0,5)
	enddo
	miner31=100000
	miner32=100000
	miner51=100000
	miner52=100000
	h31=2
	h32=2
	h51=2
	h52=2
	t=1.56
	h=0.01
	d31=0
	d32=0
	d51=0
	d52=0
	do n=0,20
		diff31=(tan(t+h)-tan(t-h))/(2*h)
		d31(n,0)=diff31
		diff32=((tan(t+h))-(2*(tan(t)))+tan(t-h))/(h*h)
		d32(n,0)=diff32
		diff51=((tan(t-2*h))-(8*(tan(t-h)))+(8*(tan(t+h)))-(tan(t+2*h)))/(12*h)
		d51(n,0)=diff51
		diff52=(-(tan(t-2*h))+(16*tan(t-h))-(30*(tan(t)))+(16*(tan(t+h)))-tan(t+(2*h)))/(12*h*h)
		d52(n,0)=diff52
		e31=abs(diff31-(1/cos(t))*(1/cos(t)))
		e32=abs(diff32-(2*(1/cos(t))*(1/cos(t))*tan(t)))
		e51=abs(diff51-(1/cos(t))*(1/cos(t)))
		e52=abs(diff52-(2*(1/cos(t))*(1/cos(t))*tan(t)))
		print *,diff31,diff32,diff51,diff52
		if (e31<miner31) then
			miner31=e31
			h31=h
		endif
		if (e32<miner32) then
			miner32=e32
			h32=h
		endif
		if (e51<miner51) then
			miner51=e51
			h51=h
		endif
		if (e52<miner52) then
			miner52=e52
			h52=h
		endif
		h=h/2
	enddo
	print *,(1/cos(t))*(1/cos(t)),2*(1/cos(t))*(1/cos(t))*tan(t)
	PRINT *,"OPTIMAL h for 1st derivative 3 point is",h31, "with error",miner31
	PRINT *,"OPTIMAL h for 2nd derivative 3 point is",h32, "with error",miner32
	PRINT *,"OPTIMAL h for 1st derivative 5 point is",h51, "with error",miner51
	PRINT *,"OPTIMAL h for 2nd derivative 5 point is",h52, "with error",miner52
	do i=1,20
		do j=1,i-1
			d31(i,j)=d31(i,j-1)+(d31(i,j-1)-d31(i-1,j-1))/((4**(j))-1)
		enddo
	enddo
	do i=0,5
		print *,(d31(i,j), j=0,5)
	enddo
	do i=1,20
		do j=1,i-1
			d32(i,j)=d32(i,j-1)+(d32(i,j-1)-d32(i-1,j-1))/((4**(j))-1)
		enddo
	enddo
	do i=0,5
		print *,(d32(i,j), j=0,5)
	enddo
	do i=1,20
		do j=1,i-1
			d51(i,j)=d51(i,j-1)+(d51(i,j-1)-d51(i-1,j-1))/((4**(j))-1)
		enddo
	enddo
	do i=0,5
		print *,(d51(i,j), j=0,5)
	enddo
	do i=1,20
		do j=1,i-1
			d52(i,j)=d52(i,j-1)+(d52(i,j-1)-d52(i-1,j-1))/((4**(j))-1)
		enddo
	enddo
	do i=0,5
		print *,(d52(i,j), j=0,5)
	enddo
end program difference
		
		
	
	

