program vector_test
	implicit none

	real, allocatable, dimension(:) :: A,B,C	
	real :: t1,t2
	integer :: n=100000000
	integer :: i
	allocate(A(1:n),B(1:n),C(1:n))
	call random_number(harvest=A(1:n))
	call random_number(harvest=B(1:n))
	call cpu_time(t1)
	do i=1,n	
		C(i)=A(i)+B(i)
	enddo
	call cpu_time(t2)

	write (*,*) 'Elapsed CPU time = ', t2-t1

end 
