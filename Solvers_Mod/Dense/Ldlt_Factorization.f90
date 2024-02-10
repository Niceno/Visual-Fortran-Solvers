!==============================================================================!
  subroutine Solvers_Mod_Dense_Ldlt_Factorization(LDL, A)
!------------------------------------------------------------------------------!
!>  Computes LDL' decomposition on square (full) matrices.
!------------------------------------------------------------------------------!
!   Called by:                                                                 !
!   - Solvers_Mod_Ldlt_Solver                                                  !
!------------------------------------------------------------------------------!
!   The resulting matrix contains L and L' which are stored without their  !
!   diagonals, since they are assumed to be 1, but the diagonal term of LDL      !
!   holds the D matrix which results from LDL' factorization.                  !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Dense_Type) :: LDL  !! factorized matrix (three in one, really)
  type(Dense_Type) :: A    !! original matrix
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, k, m
  real    :: sum
!==============================================================================!

  print *, '# Factorizing square (full) matrix with LDL'' method'

  do k = 1, A % n
    sum = A % val(k,k)
    do m = max(1, k - A % bw), k - 1
      sum = sum - LDL % val(k,m) * LDL % val(k,m) * LDL % val(m,m)
    end do
    LDL % val(k,k) = sum                   ! diagonal entry, D from LDL
    do i = k + 1, min(k + A % bw, A % n)
      sum = A % val(i,k)
      do m = max(1, k - A % bw), k - 1
        sum = sum - LDL % val(m,i) * LDL % val(m,k) * LDL % val(m,m)
      end do
      LDL % val(k,i) = sum / LDL % val(k,k)  ! upper triangle, the L' here
      LDL % val(i,k) = sum / LDL % val(k,k)  ! lower triangle, the L part
    end do
  end do

  call IO % Plot_Snippet(__FILE__, 24, 38)

  end subroutine
