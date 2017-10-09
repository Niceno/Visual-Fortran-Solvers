!======================================================================!
  subroutine Backward_Substitution(x, a, b, n)
!----------------------------------------------------------------------!
  implicit none
!----------------------------------------------------------------------!
  integer              :: n
  real, dimension(n,n) :: a
  real, dimension(n)   :: x
  real, dimension(n)   :: b
!----------------------------------------------------------------------!
  integer :: i, j
  real    :: sum
!----------------------------------------------------------------------!

  do i=n,1,-1
    sum = b(i)
    do j=i+1,n
      sum = sum - a(i,j)*x(j)
    end do
    x(i) = sum/a(i,i)
  end do

  end subroutine Backward_Substitution
