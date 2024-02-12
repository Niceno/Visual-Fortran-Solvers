!==============================================================================!
  subroutine Solvers_Mod_Dense_Forward_Substitution(x, L, b)
!------------------------------------------------------------------------------!
!>  Performs forward substitution on a square (full) matrix.
!   It will address only elements in lower trinangular part though.            !
!------------------------------------------------------------------------------!
!   Called by:                                                                 !
!   - Solvers_Mod_Cholesky                                                     !
!   - Solvers_Mod_Lu                                                           !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:) :: x  !! resulting vector
  type(Dense_Type)   :: L  !! factorized matrix, should be L in the caller
  real, dimension(:) :: b  !! right hand side vector
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, n, bw
  real    :: sum
!==============================================================================!

  ! Take some aliases
  n  = L % n
  bw = L % bw

  ! Here, i > j, therfore it is a lower matrix
  do i = 1, n
    sum = b(i)
    do j = max(1, i - bw), i-1
      sum = sum - L % val(i,j)*x(j)  ! straightforward for sparse matrix
    end do
    x(i) = sum / L % val(i,i)
  end do

  call IO % Plot_Snippet(__FILE__, 23, 29)

  end subroutine
