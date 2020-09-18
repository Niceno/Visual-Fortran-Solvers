!==============================================================================!
  subroutine Solvers_Mod_Forward_Substitution(x, f, b)
!------------------------------------------------------------------------------!
!   Performs forward substitution on a full matrix.                            !
!   It will address only elements in lower trinangular part though.            !
!                                                                              !
!   Called by:                                                                 !
!   - Demo_Mod_Cholesky_Solver                                                 !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:)   :: x
  real, dimension(:,:) :: f
  real, dimension(:)   :: b
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, n
  real    :: sum
!==============================================================================!

  n = size(f,1)  ! some checks would be possible

  do i = 1, n
    sum = b(i)
    do j = 1, i-1
      sum = sum - f(i,j)*x(j)  ! straightforward for compressed row format
    end do
    x(i) = sum / f(i,i)
  end do

  end subroutine
