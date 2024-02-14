!==============================================================================!
  subroutine Solvers_Mod_Dense_Ldlt_Solution(x, LD, b)
!------------------------------------------------------------------------------!
!>  Solves system based on LDL' decomposition.
!------------------------------------------------------------------------------!
!   Called by:                                                                 !
!   - Solvers_Mod_Ldlt                                                         !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:) :: x   !! solution vector
  type(Dense_Type)   :: LD  !! factorized matrix
  real, dimension(:) :: b   !! right hand side vector
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, n, bw
  real    :: sum
!==============================================================================!

  n  = LD % n  ! some checks would be possible
  bw = LD % bw

  ! Forward substitution (j < i =--> L)
  do i = 1, n
    sum = b(i)
    do j = max(1, i - bw), i-1
      sum = sum - LD % val(i,j) * x(j)
    end do
    x(i) = sum
  end do

  ! Treat the diagonal term
  do i = 1, n
    x(i) = x(i) / LD % val(i,i)
  end do

  ! Backward substitution (j > i =--> U; use L but transposed (see j,i))
  do i = n, 1, -1
    sum = x(i)
    do j = i+1, min(i + bw, n)
      sum = sum - LD % val(j,i) * x(j)
    end do
    x(i) = sum
  end do

  call IO % Plot_Snippet(__FILE__, 23, 43)

  end subroutine
