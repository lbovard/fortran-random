! Program to compare matrix multiplication by various methods
! In other words, use mat mul!

program matrix_mulitply
        implicit none
        real, allocatable, dimension(:,:) :: A,B,C
        integer :: n=1024
        integer :: i,j,k
        real :: t1,t2

        allocate(A(1:n,1:n),B(1:n,1:n),C(1:n,1:n))
        call random_number( harvest=A(1:n,1:n))
        call random_number( harvest=B(1:n,1:n))         
       
        call cpu_time(t1) 
        call  multiply_matrix_ijk(A,B,C,n)     
        call cpu_time(t2)
        write ( *, * ) 'Elapsed CPU time for ijk = ', t2 - t1
        call cpu_time(t1) 
        call  multiply_matrix_ikj(A,B,C,n)     
        call cpu_time(t2)	
!	call print_matrix(C,n)

        write ( *, * ) 'Elapsed CPU time for ikj = ', t2 - t1
        call cpu_time(t1) 
        call  multiply_matrix_jik(A,B,C,n)     
        call cpu_time(t2)	
        write ( *, * ) 'Elapsed CPU time for jik = ', t2 - t1
        call cpu_time(t1) 
        call  multiply_matrix_jki(A,B,C,n)     
        call cpu_time(t2)
        write ( *, * ) 'Elapsed CPU time for jki = ', t2 - t1
        call cpu_time(t1) 
        call  multiply_matrix_kij(A,B,C,n)     
        call cpu_time(t2)
        write ( *, * ) 'Elapsed CPU time for kij = ', t2 - t1
        call cpu_time(t1) 
        call  multiply_matrix_kji(A,B,C,n)      
        call cpu_time(t2)	
!	call print_matrix(C,n)
        write ( *, * ) 'Elapsed CPU time for kji = ', t2 - t1   

        call cpu_time(t1)
        call zero_matrix(C,n) 
        C=  matmul(A,B)+C       
        call cpu_time(t2)
        write ( *, * ) 'Elapsed CPU time matmul  = ', t2 - t1
end



subroutine multiply_matrix_ijk(A,B,C,n)
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: A(n,n),B(n,n)
        real, intent(out) :: C(n,n)     
        integer :: i,j,k
        call zero_matrix(C,n)
        do i=1,n
                do j=1,n
                        do k=1,n
                                C(i,j)=A(i,k)*B(k,j)+C(i,j)
                        end do
                end do
        end do
end
subroutine multiply_matrix_ikj(A,B,C,n)
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: A(n,n),B(n,n)
        real, intent(out) :: C(n,n)     
        integer :: i,j,k
        call zero_matrix(C,n)
        do i=1,n
                do k=1,n
                        do j=1,n
                                C(i,j)=A(i,k)*B(k,j)+C(i,j)
                        end do
                end do
        end do
end
subroutine multiply_matrix_jki(A,B,C,n)
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: A(n,n),B(n,n)
        real, intent(out) :: C(n,n)     
        integer :: i,j,k
        call zero_matrix(C,n)
        do j=1,n
                do k=1,n
                        do i=1,n
                                C(i,j)=A(i,k)*B(k,j)+C(i,j)
                        end do
                end do
        end do
end
subroutine multiply_matrix_jik(A,B,C,n)
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: A(n,n),B(n,n)
        real, intent(out) :: C(n,n)     
        integer :: i,j,k
        call zero_matrix(C,n)
        do j=1,n
                do i=1,n
                        do k=1,n
                                C(i,j) = A(i,k)*B(k,j)+C(i,j)
                        end do
                end do
        end do
end
subroutine multiply_matrix_kij(A,B,C,n)
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: A(n,n),B(n,n)
        real, intent(out) :: C(n,n)     
        integer :: i,j,k
        call zero_matrix(C,n)
        do k=1,n
                do i=1,n
                        do j=1,n
                                C(i,j)=A(i,k)*B(k,j)+C(i,j)
                        end do
                end do
        end do
end
subroutine multiply_matrix_kji(A,B,C,n)
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: A(n,n),B(n,n)
        real, intent(out) :: C(n,n)     
        integer :: i,j,k
        call zero_matrix(C,n)
        do k=1,n
                do j=1,n
                        do i=1,n
                                C(i,j)=A(i,k)*B(k,j)+C(i,j)
                        end do
                end do
        end do
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

subroutine zero_matrix(C,n)
        implicit none
        integer, intent(in) :: n
        integer :: i,j
        real, intent(out) :: C(n,n)
        do i=1,n
                do j=1,n
                        C(i,j)=0.d0
                end do
        end do
end
