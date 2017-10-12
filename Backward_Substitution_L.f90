!==============================================================================!
  subroutine Backward_Substitution_L(x, l, b)
!------------------------------------------------------------------------------!
!   Performs forward substitution with upper trinangular matrix.               !
!   This is the twist!                                                         !
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
!   Uses the upper triangular matrix such as this ("*" are non-zero terms):
!
!   | *             |  
!   | *  *          |
!   | *  *  *       |
!   | *  *  *  *    |
!   | *  *  *  *  * |
!
!==============================================================================!

  n = size(l,1)  ! some checks would be possible 

  do i=n,1,-1
    sum = b(i)
    do j=i+1,n
      sum = sum - l(j,i)*x(j)  ! cumbersome for compressed row format
    end do
    x(i) = sum/l(i,i)
  end do

  end subroutine Backward_Substitution_L
