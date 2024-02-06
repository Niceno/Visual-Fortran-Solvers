!==============================================================================!
  subroutine Solvers_Mod_Forward_Substitution_Dense(x, f, b)
!------------------------------------------------------------------------------!
!   Performs forward substitution on a square (full) matrix.                   !
!   It will address only elements in lower trinangular part though.            !
!                                                                              !
!   Called by:                                                                 !
!   - Solvers_Mod_Cholesky                                                     !
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

  do i = 1, n
    sum = b(i)
    do j = 1, i-1
      sum = sum - f % val(i,j)*x(j)  ! straightforward for sparse matrix
    end do
    x(i) = sum / f % val(i,i)
  end do

  end subroutine
