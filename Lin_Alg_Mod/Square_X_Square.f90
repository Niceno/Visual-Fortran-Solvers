!==============================================================================!
  subroutine Lin_Alg_Mod_Square_X_Square(c, a, b)
!------------------------------------------------------------------------------!
!   Multiplies two square (full) matrices.                                     !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Square_Type) :: c
  type(Square_Type) :: a
  type(Square_Type) :: b
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, k, n
!==============================================================================!

  n = a % n  ! some checks would be possible

  do i = 1, n
    do j = 1, n
      c % val(i,j) = 0
      do k = 1, n
        c % val(i,j) = c % val(i,j) + a % val(i,k) * b % val(k,j)
      end do
    end do
  end do

  end subroutine
