!==============================================================================!
  subroutine Solvers_Mod_Ldlt_Factorization_Square(f, a, bw)
!------------------------------------------------------------------------------!
!   Computes LDL^T decomposition on square (full) matrices.                    !
!                                                                              !
!   Called by:                                                                 !
!   - Solvers_Mod_Ldlt_Solver                                                  !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Square_Type) :: f
  type(Square_Type) :: a
  integer           :: bw  ! band width
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, k, m, n
  real    :: sum1, sum2
!==============================================================================!

  print *, '# Factorizing square (full) matrix with LDL^T method'

  n = f % n  ! some checks would be possible

  do k = 1, n
    sum1 = a % val(k,k)
    do m = max(1,k-bw), k-1
      sum1 = sum1 - f % val(k,m) * f % val(k,m) * f % val(m,m)
    end do
    f % val(k,k) = sum1
    do i = k+1, min(k+bw,n)
      sum2 = a % val(i,k)
      do m = max(1,k-bw), k-1
        sum2 = sum2 - f % val(m,i) * f % val(m,k) * f % val(m,m)
      end do
      f % val(k,i) = sum2 / f % val(k,k)
      f % val(i,k) = sum2 / f % val(k,k)  ! make it full
    end do
  end do

  end subroutine
