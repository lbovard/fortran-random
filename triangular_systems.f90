! Solve basic triangular solver routines
!
!

program trig_solver

	implicit none
	real, allocatable, dimension(:,:) :: A
	real, allocatable, dimension(:) :: x,b
	integer :: i,j

	integer :: n =10000
	allocate(A(1:n,1:n))	
	allocate(b(1:n),x(1:n))

	call random_seed
	!set-up lower triangular matrix
	do i=1,n
		do j=1,i	
			call random_number( harvest=A(i,j))	
			A(i,j)=4*A(i,j)+1.0
		end do	
		call random_number(harvest=b(i))
	end do
	call lower_solver(A,b,x,n)
end 

subroutine print_matrix(A,n)
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: A(n,n)
        integer :: i
        do i=1,n
                print*, A(i,:)
        end do
end subroutine print_matrix

subroutine print_vector(A,n)

	implicit none
	
	integer, intent(in) :: n
	real, intent(in) :: A(n)

	print*, A(1:n)

end subroutine print_vector

subroutine lower_solver(A,b,x,n)
	implicit none
	integer, intent(in) :: n
	real, intent(in) :: A(n,n), b(n)
	real, intent(ouddt) :: x(n)
	integer :: i,j

	do i=n,2,-1
		x(i)=b(i)/A(i,i)
		do j=1,i-1
			x(j)=b(j)-b(i)*A(j,i)
		end do
	end do	
	x(1)=b(1)/A(1,1)
end subroutine lower_solver

