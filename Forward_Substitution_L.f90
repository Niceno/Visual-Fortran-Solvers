!==============================================================================!
  subroutine Forward_Substitution_L(x, l, b)
!------------------------------------------------------------------------------!
!   Performs forward substitution with lower trinangular matris.               !
!   It is the default!                                                         !
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  real, dimension(:)   :: x
  real, dimension(:,:) :: l
  real, dimension(:)   :: b
!------------------------------------------------------------------------------!
  integer :: i, j, n
  real    :: sum
!------------------------------------------------------------------------------!
!   Uses the lower triangular matrix such as this ("*" are non-zero terms):
!
!   | *             |  
!   | *  *          |
!   | *  *  *       |
!   | *  *  *  *    |
!   | *  *  *  *  * |
!
!==============================================================================!

  n = size(l,1)  ! some checks would be possible

  do i=1,n
    sum = b(i)
    do j=1,i-1
      sum = sum - l(i,j)*x(j)  ! straightforward for compressed row format
    end do
    x(i) = sum / l(i,i)
  end do

  end subroutine Forward_Substitution_L
