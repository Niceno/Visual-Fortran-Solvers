!==============================================================================!
  subroutine Solvers_Mod_Sparse_Backward_Substitution(x, F, b)
!------------------------------------------------------------------------------!
!>  Performs backward substitution using a sparse matrix.
!------------------------------------------------------------------------------!
!   Called by:                                                                 !
!   - Solvers_Mod_Incomplete_Cholesky                                          !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:) :: x  !! resulting vector
  type(Sparse_Type)  :: F  !! factorized matrix, should be U in the caller
  real, dimension(:) :: b  !! right hand side vector
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, i_j, n
  real    :: sum
!==============================================================================!

  n = F % n      ! some checks would be possible

  do i = n, 1, -1
    sum = b(i)
    do i_j = F % dia(i) + 1, F % row(i + 1) - 1
      j = F % col(i_j)
      sum = sum - F % val(i_j) * x(j)
    end do
    x(i) = sum / F % val( F % dia(i) )
  end do

  end subroutine
