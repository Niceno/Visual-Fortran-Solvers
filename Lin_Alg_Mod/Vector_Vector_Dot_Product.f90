!==============================================================================!
  subroutine Lin_Alg_Mod_Vector_Vector_Dot_Product(sum, x, y)
!------------------------------------------------------------------------------!
!   Computes vector dot product.                                               !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real               :: sum
  real, dimension(:) :: x
  real, dimension(:) :: y
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, n
!==============================================================================!

  n = size(x, 1)  ! some checks would be possible

  !$acc parallel present(sum)
  sum = 0
  !$acc end parallel

  !$acc  parallel loop reduction(+:sum)  &
  !$acc& present(x, y, sum)
  do i = 1, n
    sum = sum + x(i) * y(i)
  end do

  end subroutine
