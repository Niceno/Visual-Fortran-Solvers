!==============================================================================!
  subroutine Solvers_Mod_Dense_Cholesky_Factorization(F, A)
!------------------------------------------------------------------------------!
!>  Computes Cholesky decomposition on square (full) matrices.
!------------------------------------------------------------------------------!
!   Called by:                                                                 !
!   - Solvers_Mod_Cholesky                                                     !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Dense_Type) :: F
  type(Dense_Type) :: A
  integer          :: i, k, m
  real             :: sum
!==============================================================================!

  print *, '# Factorizing square (full) matrix with Cholesky method'

  do k = 1, A % n
    sum = A % val(k,k)
    do m = max(1, k - A % bw), k - 1
      sum = sum - F % val(k,m)**2  ! straightforward for sparse matrix
    end do
    F % val(k,k) = sqrt(sum)
    do i = k + 1, min(k + A % bw, A % n)
      sum = A % val(i,k)
      do m = max(1, k - A % bw), k - 1
        sum = sum - F % val(m,i)*F % val(m,k)  ! straighforward for sparse
      end do
      F % val(k,i) = sum / F % val(k,k)
      F % val(i,k) = sum / F % val(k,k)  ! make it full
    end do
  end do

  end subroutine
