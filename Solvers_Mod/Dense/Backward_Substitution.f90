!==============================================================================!
  subroutine Solvers_Mod_Dense_Backward_Substitution(x, F, b)
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
  type(Dense_Type)   :: F  !! factorized matrix, should be U in the caller
  real, dimension(:) :: b  !! right hand side vector
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, n
  real    :: sum
!==============================================================================!

  n = F % n  ! some checks would be possible

  do i = n, 1, -1
    sum = b(i)
    do j = i+1, n
      sum = sum - F % val(i,j)*x(j)  ! straighforward for sparse matrix
    end do
    x(i) = sum / F % val(i,i)
  end do

  end subroutine
