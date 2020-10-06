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
  integer :: i, j, k
  real    :: sum1
!==============================================================================!

  ! Forward substitutionn
  !$acc  parallel loop seq                               &
  !$acc& present(a, a % row, a % col, a % dia, a % val)  &
  !$acc& present(d, d % row, d % col, d % dia, d % val)  &
  !$acc& present(x, b)
  do i = 1, n
    sum1 = b(i)
    !$acc loop vector reduction(+:sum1)
    do j = a % row(i), a % dia(i) - 1  ! only the lower triangular
      k = a % col(j)
      sum1 = sum1 - a % val(j) * x(k)
    end do
    x(i) = sum1 * d % val(d % dia(i))         ! BUG ?
  end do

  !$acc  parallel loop                                   &
  !$acc& present(d, d % row, d % col, d % dia, d % val)  &
  !$acc& present(x)
  do i = 1, n
    x(i) = x(i) / ( d % val(d % dia(i)) )
  end do

  ! Backward substitution
  !$acc  parallel loop seq                               &
  !$acc& present(a, a % row, a % col, a % dia, a % val)  &
  !$acc& present(d, d % row, d % col, d % dia, d % val)  &
  !$acc& present(x, b)
  do i = n, 1, -1
    sum1=x(i)
    !$acc loop vector reduction(+:sum1)
    do j = a % dia(i) + 1, a % row(i+1) - 1 ! upper triangular
      k = a % col(j)
      sum1 = sum1 - a % val(j) * x(k)
    end do
    x(i) = sum1 * d % val(d % dia(i))               ! BUG ?
  end do

  end subroutine
