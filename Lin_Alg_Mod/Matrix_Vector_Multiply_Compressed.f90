!==============================================================================!
  subroutine Lin_Alg_Mod_Matrix_Vector_Multiply_Compressed(y, c, x)
!------------------------------------------------------------------------------!
!   Computes matrix vector product with matrix in compressed row format.       !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:) :: y
  type(Matrix)       :: c
  real, dimension(:) :: x
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, i_j, n
!==============================================================================!

  n = c % n  ! some checks would be possible

  do i = 1, n
    y(i) = 0.0
    do i_j = c % row(i), c % row(i+1) - 1
      j = c % col(i_j)
      y(i) = y(i) + c % val(i_j) * x(j)
    end do
  end do

  end subroutine
