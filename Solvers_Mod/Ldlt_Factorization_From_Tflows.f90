!==============================================================================!
  subroutine Solvers_Mod_Ldlt_Factorization_From_Tflows(f, a)
!------------------------------------------------------------------------------!
!   Forms preconditioning matrix "f" from provided matrix "a".                 !
!                                                                              !
!   Called by:                                                                 !
!   - Solvers_Mod_Incomplete_Ldlt_From_Tflows                                  !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Sparse_Type) :: f
  type(Sparse_Type) :: a
!-----------------------------------[Locals]-----------------------------------!
  real     :: sum
  integer  :: n, i, ij, j
!==============================================================================!

  n = a % n

  do i = 1,n
    sum = a % val(a % dia(i))         ! take diaginal entry
    do ij = a % row(i), a % dia(i)-1  ! only lower traingular
      j = a % col(ij)                 ! fetch the column
      sum = sum - f % val(f % dia(j)) * a % val(ij) * a % val(ij)
    end do

    ! This is only the diagonal from LDL decomposition
    f % val(f % dia(i)) = 1.0 / sum
  end do

  end subroutine
