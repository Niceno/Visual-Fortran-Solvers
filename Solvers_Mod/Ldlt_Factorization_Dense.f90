!==============================================================================!
  subroutine Solvers_Mod_Ldlt_Factorization_Dense(F, A, bw)
!------------------------------------------------------------------------------!
!   Computes LDL^T decomposition on square (full) matrices.                    !
!                                                                              !
!   Called by:                                                                 !
!   - Solvers_Mod_Ldlt_Solver                                                  !
!------------------------------------------------------------------------------!
!   The resulting matrix, F, contains L and L' which are stored without their  !
!   diagonals, since they are assumed to be 1, but the diagonal term of F      !
!   holds the D matrix which results from LDL' factorization.                  !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Dense_Type) :: F   !! factorized matrix
  type(Dense_Type) :: A   !! original matrix
  integer          :: bw  !! band width
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, k, m, n
  real    :: sum
!==============================================================================!

  print *, '# Factorizing square (full) matrix with LDL^T method'

  n = F % n  ! some checks would be possible

  do k = 1, n
    sum = A % val(k,k)
    do m = max(1,k-bw), k-1
      sum = sum - F % val(k,m) * F % val(k,m) * F % val(m,m)
    end do
    F % val(k,k) = sum                   ! diagonal entry, D from LDL
    do i = k+1, min(k+bw,n)
      sum = A % val(i,k)
      do m = max(1,k-bw), k-1
        sum = sum - F % val(m,i) * F % val(m,k) * F % val(m,m)
      end do
      F % val(k,i) = sum / F % val(k,k)  ! upper triangle, the L' here
      F % val(i,k) = sum / F % val(k,k)  ! lower triangle, the L part
    end do
  end do

  end subroutine
