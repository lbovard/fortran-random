program blah
	implicit none
	real, dimension(10,10) :: A
	
	A=1.0
	call print_matrix(A,10)
end

subroutine print_matrix(A,n)
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: A(n,n)
        integer :: i
        do i=1,n
                print*, A(i,:)
        end do
end 

