!==============================================================================!
  subroutine Solvers_Mod_Prec_Solve(n, nb, a, d, x, b)
!------------------------------------------------------------------------------!
!   Solves the preconditioning system [d]{x}={b}                               !
!                                                                              !
!   Called by:                                                                 !
!   - Demo_Mod_Ldlt_Solver_From_Tflows                                         !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer      :: n, nb
  type(Matrix) :: a
  type(Matrix) :: d
  real         :: x(-nb:n), b(n)
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, k
  real    :: sum1
!==============================================================================!

  ! Forward substitutionn
  do i = 1, n
    sum1=b(i)
    do j = a % row(i), a % dia(i)-1  ! only the lower triangular
      k = a % col(j)
      sum1 = sum1- a % val(j)*x(k)
    end do
    x(i) = sum1 * d % val(d % dia(i))         ! BUG ?
  end do

  do i = 1, n
    x(i) = x(i) / ( d % val(d % dia(i)) )
  end do

  ! Backward substitution
  do i = n, 1, -1
    sum1=x(i)
    do j = a % dia(i)+1, a % row(i+1)-1 ! upper triangular
      k = a % col(j)
      sum1 = sum1 - a % val(j)*x(k)
    end do
    x(i) = sum1 * d % val(d % dia(i))               ! BUG ?
  end do

  end subroutine
