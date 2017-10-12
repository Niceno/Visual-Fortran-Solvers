!==============================================================================!
  subroutine Forward_Substitution_U(x, u, b)
!------------------------------------------------------------------------------!
!   Performs forward substitution with upper trinangular matrix.               !
!   This is the twist!                                                         !
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  real, dimension(:)   :: x
  real, dimension(:,:) :: u
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

  n = size(u,1)  ! some checks would be possible

  do i=1,n
    sum = b(i)
    do j=1,i-1
      sum = sum - u(j,i)*x(j)
    end do
    x(i) = sum / u(i,i)
  end do

  end subroutine Forward_Substitution_U
