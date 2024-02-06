!==============================================================================!
  subroutine Solvers_Mod_Ldlt_Solution_Sparse(x, F, b)
!------------------------------------------------------------------------------!
!   Performs forward substitution using a sparse matrix.                       !
!                                                                              !
!   Called by:                                                                 !
!   - Solvers_Mod_Incomplete_Ldlt                                              !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:) :: x  !! solution vector
  type(Sparse_Type)  :: F  !! factorized matrix
  real, dimension(:) :: b  !! right hand side vector
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, ij, n
  real    :: sum
!==============================================================================!

  n = F % n      ! some checks would be possible

  ! Forward substitution
  do i = 1, n
    sum = b(i)
    do ij = F % row(i), F % dia(i) - 1
      j = F % col(ij)
      sum = sum - F % val(ij) * x(j)
    end do
    x(i) = sum
  end do

  ! Treat the diagonal term
  do i = 1, n
    x(i) = x(i) / F % val( F % dia(i) )
  end do

  do i=n,1,-1
    sum = x(i)
    do ij = F % dia(i) + 1, F % row(i + 1) - 1
      j = F % col(ij)
      sum = sum - F % val(ij) * x(j)
    end do
    x(i) = sum
  end do

  end subroutine
