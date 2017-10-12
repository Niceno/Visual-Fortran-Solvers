!==============================================================================!
  subroutine Cholesky_Factorization_L(l, a)
!------------------------------------------------------------------------------!
!   Computes lower trianguar Cholesky decomposition.  It is the default!       !
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  real, dimension(:,:) :: l
  real, dimension(:,:) :: a
!------------------------------------------------------------------------------!
  integer :: i, k, m, n
  real    :: sum1, sum2
!------------------------------------------------------------------------------!
!   Creates lower triangular matrix such as this ("*" are non-zero terms):
!
!   | *             |  
!   | *  *          |
!   | *  *  *       |
!   | *  *  *  *    |
!   | *  *  *  *  * |
!
!==============================================================================!

  n = size(a,1)  ! some checks would be possible

  do k=1,n
    sum1 = a(k,k)
    do m=1,k-1
      sum1 = sum1 - l(k,m)**2.0  ! straightforward for compressed row format
    end do
    l(k,k) = sqrt(sum1)
    do i=k+1,n
      sum2 = a(i,k)
      do m=1,k-1
        sum2 = sum2 - l(i,m)*l(k,m)  ! cumbersome for compressed row format
      end do
      l(i,k) = sum2/l(k,k)
    end do
  end do

  end subroutine Cholesky_Factorization_L
