!==============================================================================!
  subroutine Solvers_Mod_Cholesky_Factorization_Dense(F, A, bw)
!------------------------------------------------------------------------------!
!   Computes Cholesky decomposition on square (full) matrices.                 !
!                                                                              !
!   Called by:                                                                 !
!   - Solvers_Mod_Cholesky                                                     !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Dense_Type) :: F
  type(Dense_Type) :: A
  integer          :: bw  ! band width
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, k, m, n
  real    :: sum
!==============================================================================!

  print *, '# Factorizing square (full) matrix with Cholesky method'

  n = A % n  ! some checks would be possible

  do k = 1, n
    sum = A % val(k,k)
    do m = max(1,k-bw), k-1
      sum = sum - F % val(k,m)**2  ! straightforward for sparse matrix
    end do
    F % val(k,k) = sqrt(sum)
    do i = k+1, min(k+bw,n)
      sum = A % val(i,k)
      do m = max(1,k-bw), k-1
        sum = sum - F % val(m,i)*F % val(m,k)  ! straighforward for sparse
      end do
      F % val(k,i) = sum / F % val(k,k)
      F % val(i,k) = sum / F % val(k,k)  ! make it full
    end do
  end do

  end subroutine
