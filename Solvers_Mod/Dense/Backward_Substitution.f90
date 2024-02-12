!==============================================================================!
  subroutine Solvers_Mod_Dense_Backward_Substitution(x, U, b)
!------------------------------------------------------------------------------!
!>  Performs backward substitution on a square (full) matrix.                  !
!>  It will address only elements in upper trinangular part.                   !
!------------------------------------------------------------------------------!
!   Note:                                                                      !
!                                                                              !
!   * It is called called by:                                                  !
!     - Solvers_Mod_Cholesky                                                   !
!     - Solvers_Mod_Gauss                                                      !
!     - Solvers_Mod_Lu                                                         !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:) :: x  !! resulting vector
  type(Dense_Type)   :: U  !! factorized matrix, should be U in the caller
  real, dimension(:) :: b  !! right hand side vector
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, n, bw
  real    :: sum
!==============================================================================!

  ! Take some aliases
  n  = U % n
  bw = U % bw

  ! Here, j > i, therfore it is an upper matrix
  do i = n, 1, -1
    sum = b(i)
    do j = i+1, min(i + bw, n)
      sum = sum - U % val(i,j) * x(j)  ! straighforward for sparse matrix
    end do
    x(i) = sum / U % val(i,i)
  end do

  call IO % Plot_Snippet(__FILE__, 26, 32)

  end subroutine
