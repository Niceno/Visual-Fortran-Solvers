!==============================================================================!
  subroutine Lin_Alg_Mod_Dense_X_Vector(y, A, x)
!------------------------------------------------------------------------------!
!>  Computes matrix vector product where matrix is dense (full).
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:) :: y
  type(Dense_Type)   :: A
  real, dimension(:) :: x
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j
!==============================================================================!

  Assert(A % n .eq. size(x))
  Assert(A % n .eq. size(y))

  do i = 1, A % n
    y(i) = 0.0
    do j = 1, A % n
      y(i) = y(i) + A % val(i,j) * x(j)
    end do
  end do

  end subroutine
