!==============================================================================!
  subroutine Solvers_Mod_Cholesky_Factorization_Square(f, a, bw)
!------------------------------------------------------------------------------!
!   Computes Cholesky decomposition on square (full) matrices.                 !
!                                                                              !
!   Called by:                                                                 !
!   - Solvers_Mod_Cholesky                                                     !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Square_Type) :: f
  type(Square_Type) :: a
  integer           :: bw  ! band width
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, k, m, n
  real    :: sum
!==============================================================================!

  print *, '# Factorizing square (full) matrix with Cholesky method'

  n = a % n  ! some checks would be possible

  do k = 1, n
    sum = a % val(k,k)
    do m = max(1,k-bw), k-1
      sum = sum - f % val(k,m)**2  ! straightforward for sparse matrix
    end do
    f % val(k,k) = sqrt(sum)
    do i = k+1, min(k+bw,n)
      sum = a % val(i,k)
      do m = max(1,k-bw), k-1
        sum = sum - f % val(m,i)*f % val(m,k)  ! straighforward for sparse
      end do
      f % val(k,i) = sum / f % val(k,k)
      f % val(i,k) = sum / f % val(k,k)  ! make it full
    end do
  end do

  end subroutine
