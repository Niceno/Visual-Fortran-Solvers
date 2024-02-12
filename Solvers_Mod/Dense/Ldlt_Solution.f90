!==============================================================================!
  subroutine Solvers_Mod_Dense_Ldlt_Solution(x, F, b)
!------------------------------------------------------------------------------!
!>  Solves system based on LDL' decomposition.
!------------------------------------------------------------------------------!
!   Called by:                                                                 !
!   - Solvers_Mod_Ldlt                                                         !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:) :: x  !! solution vector
  type(Dense_Type)   :: F  !! factorized matrix
  real, dimension(:) :: b  !! right hand side vector
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, n, bw
  real    :: sum
!==============================================================================!

  n  = F % n  ! some checks would be possible
  bw = F % bw

  ! Forward substitutions
  do i = 1, n
    sum = b(i)
    do j = max(1, i - bw), i-1
      sum = sum - F % val(i,j) * x(j)  ! straightforward for compressed row format
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
    do j = i+1, min(i + bw, n)
      sum = sum - F % val(i,j) * x(j)  ! straighforward for compressed row format
    end do
    x(i) = sum
  end do

  call IO % Plot_Snippet(__FILE__, 21, 42)

  end subroutine
