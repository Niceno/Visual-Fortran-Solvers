!==============================================================================!
  subroutine Solvers_Mod_Ldlt_Solution_From_Tflows(n, nb, a, d, x, b)
!------------------------------------------------------------------------------!
!   Solves the preconditioning system [d]{x}={b}                               !
!                                                                              !
!   Called by:                                                                 !
!   - Solvers_Mod_Incomplete_Ldlt_From_Tflows                                  !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer           :: n, nb
  type(Sparse_Type) :: a
  type(Sparse_Type) :: d
  real              :: x(-nb:n), b(n)
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, ij, j
  real    :: sum
!==============================================================================!

  ! Forward substitutionn
  do i = 1, n
    sum = b(i)
    do ij = a % row(i), a % dia(i) - 1  ! only the lower triangular
      j = a % col(ij)                   ! fetch the column
      sum = sum - a % val(ij) * x(j)
    end do
    x(i) = sum * d % val(d % dia(i))         ! BUG ?
  end do

  do i = 1, n
    x(i) = x(i) / ( d % val(d % dia(i)) )
  end do

  ! Backward substitution
  do i = n, 1, -1
    sum = x(i)
    do ij = a % dia(i) + 1, a % row(i+1) - 1  ! upper triangular
      j = a % col(ij)                         ! fetch the column
      sum = sum - a % val(ij) * x(j)
    end do
    x(i) = sum * d % val(d % dia(i))               ! BUG ?
  end do

  end subroutine
