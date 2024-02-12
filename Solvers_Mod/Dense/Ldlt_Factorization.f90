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
  integer :: i, k, s, n, bw
  real    :: sum
!==============================================================================!

  print *, '# Factorizing square (full) matrix with LDL'' method'

  ! Take some aliases
  n  = A % n
  bw = A % bw

  ! Initialize the values
  LDL % val(:,:) = 0.0

  ! Perform the factorization
  do k = 1, n

    ! Work out the diagonal D
    sum = 0.0
    do s = max(1, k - bw), k - 1
      sum = sum + LDL % val(k,s)**2 * LDL % val(s,s)
      call IO % Plot_Dense("factorization", LDL, B=A, src1=(/k,s,GREEN/), src2=(/s,s,GREEN2/))
    end do
    LDL % val(k,k) = A % val(k,k) - sum      ! diagonal entry, D from LDL
    call IO % Plot_Dense("factorization", LDL, B=A, targ=(/k,k,PINK2/))

    ! Work out the diagonal L
    do i = k + 1, min(k + bw, n)
      sum = 0.0
      do s = max(1, k - bw, i - bw), k - 1
        sum = sum + LDL % val(i,s) * LDL % val(k,s) * LDL % val(s,s)
        call IO % Plot_Dense("factorization", LDL, B=A, src1=(/i,s,GREEN/), src2=(/k,s,GREEN2/), src3=(/s,s,GREEN4/))
      end do
      LDL % val(i,k) = (A % val(i,k) - sum) / LDL % val(k,k)
      call IO % Plot_Dense("factorization", LDL, B=A, targ=(/i,k,PINK2/))
    end do
  end do

  call IO % Plot_Snippet(__FILE__, 28, 42)

  end subroutine
