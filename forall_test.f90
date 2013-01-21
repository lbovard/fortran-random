! some discussion
! http://stackoverflow.com/questions/8602596/fortran-forall-restrictions
! this test seems to indicate that forall is just as fast as nested do loops
program foralltest
  implicit none
  real(kind=8),  dimension(:,:),allocatable :: A  
  real(kind=8) :: t1,t2
  integer :: i,j,N

  N=10000
  allocate(A(N,N))
  
  call cpu_time(t1)
  forall(i=1:N,j=1:N) A(j,i)=i+j
  call cpu_time(t2)
  print *, 'Elapsed time = ', t2-t1

  A=0
  call cpu_time(t1)
  forall(j=1:N,i=1:N) A(j,i)=i+j
  call cpu_time(t2)
  print *, 'Elapsed time = ', t2-t1

  A=0
  call cpu_time(t1) 
  do i=1,N
    do j=1,N
      A(j,i)=i+j
    end do
  end do
  call cpu_time(t2)
  print *, 'Elapsed time = ', t2-t1
end program foralltest
