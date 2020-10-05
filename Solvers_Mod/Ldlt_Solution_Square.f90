!==============================================================================!
  subroutine Solvers_Mod_Ldlt_Solution_Square(x, f, b)
!------------------------------------------------------------------------------!
!   Solves system based on LDL^T decomposition.                                !
!                                                                              !
!   Called by:                                                                 !
!   - Solvers_Mod_Ldlt                                                         !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:) :: x
  type(Square_Type)  :: f
  real, dimension(:) :: b
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, n
  real    :: sum
!==============================================================================!

  n = f % n  ! some checks would be possible

  ! Forward substitutions
  do i = 1, n
    sum = b(i)
    do j=1,i-1
      sum = sum - f % val(i,j)*x(j)  ! straightforward for compressed row format
    end do
    x(i) = sum
  end do

  ! Treat the diagonal term
  do i = 1, n
    x(i) = x(i) / f % val(i,i)
  end do

  ! Backward substitution
  do i = n, 1, -1
    sum = x(i)
    do j = i+1, n
      sum = sum - f % val(i,j)*x(j)  ! straighforward for compressed row format
    end do
    x(i) = sum
  end do

  end subroutine
