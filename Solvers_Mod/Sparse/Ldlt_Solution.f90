!==============================================================================!
  subroutine Solvers_Mod_Sparse_Ldlt_Solution(x, LDL, b)
!------------------------------------------------------------------------------!
!>  Performs forward substitution using a sparse matrix.
!------------------------------------------------------------------------------!
!   Called by:                                                                 !
!   - Solvers_Mod_Incomplete_Ldlt                                              !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:) :: x  !! solution vector
  type(Sparse_Type)  :: LDL  !! factorized matrix
  real, dimension(:) :: b  !! right hand side vector
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, ij, n
  real    :: sum
!==============================================================================!

  n = LDL % n      ! some checks would be possible

  ! Forward substitution
  do i = 1, n
    sum = b(i)
    do ij = LDL % row(i), LDL % dia(i) - 1
      j = LDL % col(ij)
      sum = sum - LDL % val(ij) * x(j)
    end do
    x(i) = sum
  end do

  ! Treat the diagonal term
  do i = 1, n
    x(i) = x(i) / LDL % val( LDL % dia(i) )
  end do

  do i=n,1,-1
    sum = x(i)
    do ij = LDL % dia(i) + 1, LDL % row(i + 1) - 1
      j = LDL % col(ij)
      sum = sum - LDL % val(ij) * x(j)
    end do
    x(i) = sum
  end do

  end subroutine
