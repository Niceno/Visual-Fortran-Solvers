!==============================================================================!
  subroutine Solvers_Mod_Dense_Ldlt_Solution(x, L, b)
!------------------------------------------------------------------------------!
!>  Solves system based on LDL' decomposition.
!------------------------------------------------------------------------------!
!   Called by:                                                                 !
!   - Solvers_Mod_Ldlt                                                         !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:) :: x  !! solution vector
  type(Dense_Type)   :: L  !! factorized matrix
  real, dimension(:) :: b  !! right hand side vector
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, n, bw
  real    :: sum
!==============================================================================!

  n  = L % n  ! some checks would be possible
  bw = L % bw

  ! Forward substitution (j < i =--> L)
  do i = 1, n
    sum = b(i)
    do j = max(1, i - bw), i-1
      sum = sum - L % val(i,j) * x(j)  ! straightforward for compressed row format
    end do
    x(i) = sum
  end do

  ! Treat the diagonal term
  do i = 1, n
    x(i) = x(i) / L % val(i,i)
  end do

  ! Backward substitution (j > i =--> U; use L but transposed (see j,i))
  do i = n, 1, -1
    sum = x(i)
    do j = i+1, min(i + bw, n)
      sum = sum - L % val(j,i) * x(j)  ! straighforward for compressed row format
    end do
    x(i) = sum
  end do

  call IO % Plot_Snippet(__FILE__, 21, 42)

  end subroutine
