!==============================================================================!
  subroutine Solvers_Mod_Sparse_Ldlt_Solution(x, LD, b)
!------------------------------------------------------------------------------!
!>  Performs forward substitution using a sparse matrix.
!------------------------------------------------------------------------------!
!   Called by:                                                                 !
!   - Solvers_Mod_Incomplete_Ldlt                                              !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:) :: x   !! solution vector
  type(Sparse_Type)  :: LD  !! factorized matrix
  real, dimension(:) :: b   !! right hand side vector
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, ij, n
  real    :: sum
!==============================================================================!

  n = LD % n      ! some checks would be possible

  ! Forward substitution (j < i =--> L)
  do i = 1, n
    sum = b(i)
    do ij = LD % row(i), LD % dia(i) - 1
      j = LD % col(ij)
      sum = sum - LD % val(ij) * x(j)
    end do
    x(i) = sum
  end do

  ! Treat the diagonal term
  do i = 1, n
    x(i) = x(i) / LD % val( LD % dia(i) )
  end do

  ! Backward substitution (j > i =--> U; use L but transposed (see j,i))
  do i = n, 1, -1
    sum = x(i)
    do ij = LD % dia(i) + 1, LD % row(i + 1) - 1
      j = LD % col(ij)
      sum = sum - LD % val(LD % mir(ij)) * x(j)
    end do
    x(i) = sum
  end do

  call IO % Plot_Snippet(__FILE__, 22, 44)

  end subroutine
