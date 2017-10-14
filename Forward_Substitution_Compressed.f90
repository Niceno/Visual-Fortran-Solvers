!==============================================================================!
  subroutine Forward_Substitution_Compressed(x, f, b)
!------------------------------------------------------------------------------!
!   Performs forward substitution using only lower trinangular matrix.         !
!   It is the same as Forward_Substitution_L                                   !
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

  do i = 1, n
    sum = b(i)
    do i_j = f % row(i), f % dia(i) - 1  
      j = f % col(i_j)
      sum = sum - f % val(i_j) * x(j)
    end do
    x(i) = sum / f % val( f % dia(i) )
  end do

  end subroutine Forward_Substitution_Compressed
