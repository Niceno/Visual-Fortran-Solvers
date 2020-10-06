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
  real     :: sum1
  integer  :: n, i, j, k
!==============================================================================!

  n = a % n

  !$acc  parallel loop seq                               &
  !$acc& present(a, a % row, a % col, a % dia, a % val)  &
  !$acc& present(f, f % row, f % col, f % dia, f % val)
  do i = 1,n
    sum1 = a % val(a % dia(i))       ! take diaginal entry
    !$acc loop vector reduction(+:sum1)
    do j = a % row(i), a % dia(i)-1  ! only lower traingular
      k = a % col(j)
      sum1 = sum1 - f % val(f % dia(k)) * a % val(j) * a % val(j)
    end do

    ! This is only the diagonal from LDL decomposition
    f % val(f % dia(i)) = 1.0 / sum1
  end do

  end subroutine
