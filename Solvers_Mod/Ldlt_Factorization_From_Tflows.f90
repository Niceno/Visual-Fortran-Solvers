!==============================================================================!
  subroutine Solvers_Mod_Ldlt_Factorization_From_Tflows(a, d)
!------------------------------------------------------------------------------!
!   Forms preconditioning matrix "d" from provided matrix "a".                 !
!                                                                              !
!   Called by:                                                                 !
!   - Solvers_Mod_Incomplete_Ldlt_From_Tflows                                  !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Sparse_Type) :: a
  type(Sparse_Type) :: d
!-----------------------------------[Locals]-----------------------------------!
  real     :: sum1
  integer  :: n, i, j, k
!==============================================================================!

  n = a % n

  do i = 1,n
    sum1 = a % val(a % dia(i))       ! take diaginal entry
    do j = a % row(i), a % dia(i)-1  ! only lower traingular
      k = a % col(j)
      sum1 = sum1 - d % val(d % dia(k)) * a % val(j) * a % val(j)
    end do

    ! This is only the diagonal from LDL decomposition
    d % val(d % dia(i)) = 1.0 / sum1
  end do

  end subroutine
