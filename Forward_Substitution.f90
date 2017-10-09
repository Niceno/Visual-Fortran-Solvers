!======================================================================!
  subroutine Forward_Substitution(x, a, b, n)
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

  do i=1,n
    sum = b(i)
    do j=1,i-1
      sum = sum - a(i,j)*x(j)
    end do
    x(i) = sum / a(i,i)
  end do

  end subroutine Forward_Substitution
