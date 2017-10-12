!==============================================================================!
  subroutine Cholesky_Factorization(l, a)
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  real, dimension(:,:) :: l
  real, dimension(:,:) :: a
!------------------------------------------------------------------------------!
  integer :: i, k, m, n
  real    :: sum1, sum2
!==============================================================================!

  n = size(a,1)  ! some checks would be possible

  do k=1,n
    sum1 = a(k,k)
    do m=1,k-1
      sum1 = sum1 - l(k,m)**2.0
    end do
    l(k,k) = sqrt(sum1)
    do i=k+1,n
      sum2 = a(i,k)
      do m=1,k-1
        sum2 = sum2 - l(i,m)*l(k,m)
      end do
      l(i,k) = sum2/l(k,k)
    end do
  end do

  end subroutine Cholesky_Factorization
