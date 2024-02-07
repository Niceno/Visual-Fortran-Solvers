!==============================================================================!
  subroutine Solvers_Mod_Tflows_Ldlt_Factorization(F, A)
!------------------------------------------------------------------------------!
!   Forms preconditioning matrix "F" from provided matrix "A".                 !
!                                                                              !
!   Called by:                                                                 !
!   - Solvers_Mod_Incomplete_Ldlt_From_Tflows                                  !
!------------------------------------------------------------------------------!
!   Two points seem to be important here:                                      !
!                                                                              !
!   1. Contrary to original LDL' algorithm, the one from T-Flow only stores    !
!      the diagonal entry in the factored matrix.  The rationale behind it is, !
!      most likely to save memory.  If you take a look at the L' and L parts   !
!      in the orginal LDL' factorization (from Ldlt_Factorization_Dense.f90):  !
!                                                                              !
!      F % val(k,i) = sum / F % val(k,k)  ! upper triangle, the L' here        !
!      F % val(i,k) = sum / F % val(k,k)  ! lower triangle, the L part         !
!                                                                              !
!      they are all just scaled with diagonal entry, so there is no need to    !
!      even store them.                                                        !
!                                                                              !
!   2. Also contrary to the original LDL' algorithm. this one stores the reci- !
!      procal of the diaginal, rather than the diagonal itself.  The reason    !
!      behind is most likely to reduce the number of divisions in the for-     !
!      ward and backward sweeps during the "solution" phase.                   !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Sparse_Type) :: F  !! factorized matrix, only diagonal is used
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

    ! This is the reciprocal of the diagonal from LDL' decomposition
    F % val(F % dia(i)) = 1.0 / sum
  end do

  end subroutine
