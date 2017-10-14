!==============================================================================!
  subroutine Backward_Substitution_Compressed(x, f, b)
!------------------------------------------------------------------------------!
!   Performs forward substitution using only upper trinangular matrix.         !
!   It is the same as Backward_Substitution_U                                  !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Matrix_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  real, dimension(:)   :: x
  type(Matrix)         :: f
  real, dimension(:)   :: b
!------------------------------------------------------------------------------!
  integer :: i, j, i_j, n
  real    :: sum
!==============================================================================!

  n = f % n      ! some checks would be possible

  do i=n,1,-1
    sum = b(i)
    do i_j = f % dia(i) + 1, f % row(i + 1) - 1  
      j = f % col(i_j)
      sum = sum - f % val(i_j) * x(j)
    end do
    x(i) = sum / f % val( f % dia(i) )
  end do

  end subroutine Backward_Substitution_Compressed
