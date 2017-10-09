!======================================================================!
  subroutine Forward_Substitution(x, a, b)
!----------------------------------------------------------------------!
  implicit none
!----------------------------------------------------------------------!
  real, dimension(:)   :: x
  real, dimension(:,:) :: a
  real, dimension(:)   :: b
!----------------------------------------------------------------------!
  integer :: i, j, n
  real    :: sum
!----------------------------------------------------------------------!

  n = size(a,1)  ! some checks are possible 

  do i=1,n
    sum = b(i)
    do j=1,i-1
      sum = sum - a(i,j)*x(j)
    end do
    x(i) = sum / a(i,i)
  end do

  end subroutine Forward_Substitution
