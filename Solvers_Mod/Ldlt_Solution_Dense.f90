!==============================================================================!
  subroutine Solvers_Mod_Ldlt_Solution_Dense(x, F, b)
!------------------------------------------------------------------------------!
!   Solves system based on LDL^T decomposition.                                !
!                                                                              !
!   Called by:                                                                 !
!   - Solvers_Mod_Ldlt                                                         !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:) :: x  !! solution vector
  type(Dense_Type)   :: F  !! factorized matrix
  real, dimension(:) :: b  !! right hand side vector
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, n
  real    :: sum
!==============================================================================!

  n = F % n  ! some checks would be possible

  ! Forward substitutions
  do i = 1, n
    sum = b(i)
    do j=1,i-1
      sum = sum - F % val(i,j)*x(j)  ! straightforward for compressed row format
    end do
    x(i) = sum
  end do

  ! Treat the diagonal term
  do i = 1, n
    x(i) = x(i) / F % val(i,i)
  end do

  ! Backward substitution
  do i = n, 1, -1
    sum = x(i)
    do j = i+1, n
      sum = sum - F % val(i,j)*x(j)  ! straighforward for compressed row format
    end do
    x(i) = sum
  end do

  end subroutine
