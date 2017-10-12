!==============================================================================!
  subroutine Backward_Substitution_U(x, u, b)
!------------------------------------------------------------------------------!
!   Performs forward substitution with upper trinangular matrix.               !
!   It is the default!                                                         !
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

  do i=n,1,-1
    sum = b(i)
    do j=i+1,n
      sum = sum - u(i,j)*x(j)  ! straighforward for compressed row format
    end do
    x(i) = sum/u(i,i)
  end do

  end subroutine Backward_Substitution_U
