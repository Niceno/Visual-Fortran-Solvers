!==============================================================================!
  subroutine Solvers_Mod_Backward_Substitution_Sparse(x, f, b)
!------------------------------------------------------------------------------!
!   Performs backward substitution using a sparse matrix.                      !
!                                                                              !
!   Called by:                                                                 !
!   - Solvers_Mod_Incomplete_Cholesky                                          !
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

  do i = n, 1, -1
    sum = b(i)
    do i_j = f % dia(i) + 1, f % row(i + 1) - 1
      j = f % col(i_j)
      sum = sum - f % val(i_j) * x(j)
    end do
    x(i) = sum / f % val( f % dia(i) )
  end do

  end subroutine
