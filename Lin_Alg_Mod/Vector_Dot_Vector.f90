!==============================================================================!
  subroutine Lin_Alg_Mod_Vector_Dot_Vector(dot, x, y)
!------------------------------------------------------------------------------!
!>  Computes vector dot product.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real               :: dot  !! result
  real, dimension(:) :: x    !! operand vector
  real, dimension(:) :: y    !! operand vector
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, n
!==============================================================================!

  n = size(x, 1)  ! some checks would be possible

  dot = 0

  do i = 1, n
    dot = dot + x(i) * y(i)
  end do

  end subroutine
