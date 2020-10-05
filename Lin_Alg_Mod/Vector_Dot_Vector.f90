!==============================================================================!
  subroutine Lin_Alg_Mod_Vector_Dot_Vector(dot, x, y)
!------------------------------------------------------------------------------!
!   Computes vector dot product.                                               !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real               :: dot
  real, dimension(:) :: x
  real, dimension(:) :: y
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, n
!==============================================================================!

  n = size(x, 1)  ! some checks would be possible

  !$acc parallel present(dot)
  dot = 0
  !$acc end parallel

  !$acc  parallel loop reduction(+:dot)  &
  !$acc& present(x, y, dot)
  do i = 1, n
    dot = dot + x(i) * y(i)
  end do

  end subroutine
