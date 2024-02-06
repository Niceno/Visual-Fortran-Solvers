!==============================================================================!
  subroutine Solvers_Mod_Ldlt_Factorization_From_Tflows(F, A)
!------------------------------------------------------------------------------!
!   Forms preconditioning matrix "F" from provided matrix "A".                 !
!                                                                              !
!   Called by:                                                                 !
!   - Solvers_Mod_Incomplete_Ldlt_From_Tflows                                  !
!------------------------------------------------------------------------------!
!   What the resulting matrix here stores, and why, is still a mystery to me.  !
!   It seems that only diagonal is stored, but it might also be the reciprocal !
!   of the diagonal (see the dense version for comparison).                    !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Sparse_Type) :: F  !! factorized matrix
  type(Sparse_Type) :: A  !! original matrix
!-----------------------------------[Locals]-----------------------------------!
  real     :: sum
  integer  :: n, i, ij, j
!==============================================================================!

  n = A % n

  do i = 1, n
    sum = A % val(A % dia(i))         ! take diaginal entry
    do ij = A % row(i), A % dia(i)-1  ! only lower traingular
      j = A % col(ij)                 ! fetch the column
      sum = sum - F % val(F % dia(j)) * A % val(ij) * A % val(ij)
    end do

    ! This is only the diagonal from LDL decomposition
    F % val(F % dia(i)) = 1.0 / sum
  end do

  end subroutine
