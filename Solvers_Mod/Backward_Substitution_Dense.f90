!==============================================================================!
  subroutine Solvers_Mod_Backward_Substitution_Dense(x, f, b)
!------------------------------------------------------------------------------!
!   Performs backward substitution on a square (full) matrix.                  !
!   It will address only elements in upper trinangular part.                   !
!                                                                              !
!   Called by:                                                                 !
!   - Solvers_Mod_Cholesky                                                     !
!   - Solvers_Mod_Gauss                                                        !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:) :: x
  type(Dense_Type)   :: f
  real, dimension(:) :: b
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, n
  real    :: sum
!==============================================================================!

  n = f % n  ! some checks would be possible

  do i = n, 1, -1
    sum = b(i)
    do j = i+1, n
      sum = sum - f % val(i,j)*x(j)  ! straighforward for sparse matrix
    end do
    x(i) = sum / f % val(i,i)
  end do

  end subroutine
