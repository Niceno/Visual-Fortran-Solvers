!==============================================================================!
  subroutine Cholesky_Factorization_U(u, a)
!------------------------------------------------------------------------------!
!   Computes upper trianguar Cholesky decomposition.  This is the twist!       !
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  real, dimension(:,:) :: u
  real, dimension(:,:) :: a
!------------------------------------------------------------------------------!
  integer :: i, k, m, n
  real    :: sum1, sum2
!------------------------------------------------------------------------------!
!   Creates upper triangular matrix such as this ("*" are non-zero terms):
!
!   | *  *  *  *  * |  
!   |    *  *  *  * |
!   |       *  *  * |
!   |          *  * |
!   |             * |
!
!==============================================================================!

  n = size(a,1)  ! some checks would be possible

  do k=1,n
    sum1 = a(k,k)
    do m=1,k-1
      sum1 = sum1 - u(m,k)**2.0
    end do
    u(k,k) = sqrt(sum1)
    do i=k+1,n
      sum2 = a(i,k)
      do m=1,k-1
        sum2 = sum2 - u(m,i)*u(m,k)
      end do
      u(k,i) = sum2/u(k,k)
    end do
  end do

  end subroutine Cholesky_Factorization_U
