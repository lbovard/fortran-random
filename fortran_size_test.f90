program size_test

	implicit none
	real, allocatable,dimension(:,:) :: A
	integer :: matsize
	print *, 'Enter the matrix size'
	read *, matsize
	allocate(A(1:matsize,1:matsize))		
	print *, 'matrix size is ', size(A,1)
 end program size_test	

