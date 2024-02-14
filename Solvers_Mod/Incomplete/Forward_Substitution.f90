!==============================================================================!
  subroutine Solvers_Mod_Sparse_Forward_Substitution(x, L, b)
!------------------------------------------------------------------------------!
!>  Performs forward substitution using a sparse matrix.
!------------------------------------------------------------------------------!
!                                                                              !
!   Called by:                                                                 !
!   - Solvers_Mod_Incomplete_Cholesky                                          !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:) :: x  !! resulting vector
  type(Sparse_Type)  :: L  !! factorized matrix, should be U in the caller
  real, dimension(:) :: b  !! right hand side vector
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, ij, n
  real    :: sum
!==============================================================================!

  n = L % n      ! some checks would be possible

  do i = 1, n
    sum = b(i)
    do ij = L % row(i), L % dia(i) - 1  ! up to diagonal
      j = L % col(ij)                   ! j < i; hence L
      Assert(j < i)
      sum = sum - L % val(ij) * x(j)
    end do
    x(i) = sum / L % val( L % dia(i) )
  end do

  end subroutine
