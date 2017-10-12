!==============================================================================!
  subroutine Backward_Substitution(x, a, b)
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  real, dimension(:)   :: x
  real, dimension(:,:) :: a
  real, dimension(:)   :: b
!------------------------------------------------------------------------------!
  integer :: i, j, n
  real    :: sum
!------------------------------------------------------------------------------!
!   Uses the upper triangular matrix such as this ("*" are non-zero terms):
!
!   | *  *  *  *  * |  
!   |    *  *  *  * |
!   |       *  *  * |
!   |          *  * |
!   |             * |
!
!==============================================================================!

  n = size(a,1)  ! some checks would be possible 

  do i=n,1,-1
    sum = b(i)
    do j=i+1,n
      sum = sum - a(i,j)*x(j)
    end do
    x(i) = sum/a(i,i)
  end do

  end subroutine Backward_Substitution
