!==============================================================================!
  subroutine Lin_Alg_Mod_Square_X_Vector(y, a, x)
!------------------------------------------------------------------------------!
!   Computes matrix vector product where matrix is full.                       !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:) :: y
  type(Square_Type)  :: a
  real, dimension(:) :: x
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, n
!==============================================================================!

  n = a % n  ! some checks would be possible

  do i = 1, n
    y(i) = 0.0
    do j = 1, n
      y(i) = y(i) + a % val(i,j) * x(j)
    end do
  end do

  end subroutine
