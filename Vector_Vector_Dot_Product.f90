!==============================================================================!
  subroutine Vector_Vector_Dot_Product(sum, x, y)
!------------------------------------------------------------------------------!
!   Computes vector dot product.                                               !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real               :: sum
  real, dimension(:) :: x
  real, dimension(:) :: y
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, n
!==============================================================================!

  n = size(x, 1)  ! some checks would be possible

  sum = 0
  do i = 1, n
    sum = sum + x(i) * y(i)
  end do

  end subroutine Vector_Vector_Dot_Product
