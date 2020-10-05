!==============================================================================!
  subroutine Solvers_Mod_Ldlt_Solution_Sparse(x, f, b)
!------------------------------------------------------------------------------!
!   Performs forward substitution using a sparse matrix.                       !
!                                                                              !
!   Called by:                                                                 !
!   - Solvers_Mod_Incomplete_Ldlt                                              !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:) :: x
  type(Sparse_Type)  :: f
  real, dimension(:) :: b
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, i_j, n
  real    :: sum
!==============================================================================!

  n = f % n      ! some checks would be possible

  ! Forward substitution
  do i = 1, n
    sum = b(i)
    do i_j = f % row(i), f % dia(i) - 1
      j = f % col(i_j)
      sum = sum - f % val(i_j) * x(j)
    end do
    x(i) = sum
  end do

  ! Treat the diagonal term
  do i = 1, n
    x(i) = x(i) / f % val( f % dia(i) )
  end do

  do i=n,1,-1
    sum = x(i)
    do i_j = f % dia(i) + 1, f % row(i + 1) - 1
      j = f % col(i_j)
      sum = sum - f % val(i_j) * x(j)
    end do
    x(i) = sum
  end do

  end subroutine
