!==============================================================================!
  subroutine Matrix_Vector_Multiply(y, a, x)
!------------------------------------------------------------------------------!
!   Computes matrix vector product where matrix is full.                       !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:)   :: y
  real, dimension(:,:) :: a
  real, dimension(:)   :: x
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, k, n
!==============================================================================!

  n = size(a, 1)  ! some checks would be possible

  do i = 1, n
    y(i) = 0.0
    do j = 1, n
      y(i) = y(i) + a(i,j) * x(j)
    end do
  end do

  end subroutine Matrix_Vector_Multiply
