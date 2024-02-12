!==============================================================================!
  subroutine Solvers_Mod_Dense_Forward_Substitution(x, L, b, d1)
!------------------------------------------------------------------------------!
!>  Performs forward substitution on a square (full) matrix.
!   It will address only elements in lower trinangular part though.            !
!------------------------------------------------------------------------------!
!   Called by:                                                                 !
!   - Solvers_Mod_Cholesky                                                     !
!   - Solvers_Mod_Lu                                                           !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:) :: x   !! resulting vector
  type(Dense_Type)   :: L   !! factorized matrix, should be L in the caller
  real, dimension(:) :: b   !! right hand side vector
  logical,  optional :: d1  !! diagonal is one, good for LU decomposition
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, n, bw
  real    :: sum
  logical :: diagonal_one = .false.
!==============================================================================!

  ! Take some aliases
  n  = L % n
  bw = L % bw

  if(present(d1)) diagonal_one = d1

  ! Here, i > j, therfore it is a lower matrix
  if(.not. diagonal_one) then
    do i = 1, n
      sum = b(i)
      do j = max(1, i - bw), i-1
        sum = sum - L % val(i,j)*x(j)
      end do
      x(i) = sum / L % val(i,i)
    end do

  ! Diagonal is equal to 1, good for LU decomposition
  else
    do i = 1, n
      sum = b(i)
      do j = max(1, i - bw), i-1
        sum = sum - L % val(i,j)*x(j)
      end do
      x(i) = sum
    end do

  end if

  call IO % Plot_Snippet(__FILE__, 23, 29)

  end subroutine
