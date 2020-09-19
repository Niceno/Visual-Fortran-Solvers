!==============================================================================!
  subroutine Solvers_Mod_Backward_Substitution(x, f, b)
!------------------------------------------------------------------------------!
!   Performs backward substitution on a full matrix.                           !
!   It will address only elements in upper trinangular part.                   !
!                                                                              !
!   Called by:                                                                 !
!   - Solvers_Mod_Cholesky                                                     !
!   - Solvers_Mod_Gauss                                                        !
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

  do i = n, 1, -1
    sum = b(i)
    do j = i+1, n
      sum = sum - f(i,j)*x(j)  ! straighforward for compressed row format
    end do
    x(i) = sum / f(i,i)
  end do

  end subroutine
