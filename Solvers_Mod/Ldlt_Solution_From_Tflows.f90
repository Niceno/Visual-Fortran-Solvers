!==============================================================================!
  subroutine Solvers_Mod_Ldlt_Solution_From_Tflows(n, nb, A, F, x, b)
!------------------------------------------------------------------------------!
!   Solves the preconditioning system [F]{x}={b}                               !
!                                                                              !
!   Called by:                                                                 !
!   - Solvers_Mod_Incomplete_Ldlt_From_Tflows                                  !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer           :: n, nb
  type(Sparse_Type) :: A
  type(Sparse_Type) :: F
  real              :: x(-nb:n), b(n)
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, ij, j
  real    :: sum
!==============================================================================!

  ! Forward substitutionn
  do i = 1, n
    sum = b(i)
    do ij = A % row(i), A % dia(i) - 1  ! only the lower triangular
      j = A % col(ij)                   ! fetch the column
      sum = sum - A % val(ij) * x(j)
    end do
    x(i) = sum * F % val(F % dia(i))         ! BUG ?
  end do

  do i = 1, n
    x(i) = x(i) / ( F % val(F % dia(i)) )
  end do

  ! Backward substitution
  do i = n, 1, -1
    sum = x(i)
    do ij = A % dia(i) + 1, A % row(i+1) - 1  ! upper triangular
      j = A % col(ij)                         ! fetch the column
      sum = sum - A % val(ij) * x(j)
    end do
    x(i) = sum * F % val(F % dia(i))               ! BUG ?
  end do

  end subroutine
