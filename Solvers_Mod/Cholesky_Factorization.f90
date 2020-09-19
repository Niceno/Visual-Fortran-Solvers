!==============================================================================!
  subroutine Solvers_Mod_Cholesky_Factorization(f, a)
!------------------------------------------------------------------------------!
!   Computes Cholesky decomposition on full matrices.                          !
!
!   Called by:                                                                 !
!   - Solvers_Mod_Cholesky                                                     !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:,:) :: f
  real, dimension(:,:) :: a
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, k, m, n
  real    :: sum1, sum2
!==============================================================================!

  n = size(a,1)  ! some checks would be possible

  do k = 1, n
    sum1 = a(k,k)
    do m = 1, k-1
      sum1 = sum1 - f(k,m)**2  ! straightforward for compressed row format
    end do
    f(k,k) = sqrt(sum1)
    do i = k+1, n
      sum2 = a(i,k)
      do m=1,k-1
        sum2 = sum2 - f(m,i)*f(m,k)  ! straighforward for compressed row format
      end do
      f(k,i) = sum2 / f(k,k)
      f(i,k) = sum2 / f(k,k)  ! make it full
    end do
  end do

  end subroutine
