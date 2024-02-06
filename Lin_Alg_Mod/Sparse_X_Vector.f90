!==============================================================================!
  subroutine Lin_Alg_Mod_Sparse_X_Vector(y, a, x)
!------------------------------------------------------------------------------!
!   Computes matrix vector product with matrix in sparse row format.           !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:) :: y
  type(Sparse_Type)  :: a
  real, dimension(:) :: x
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, ij, n
  real    :: y_i_sum
!==============================================================================!

  n = a % n  ! some checks would be possible

  do i = 1, n
    y_i_sum = 0.0
    do ij = a % row(i), a % row(i+1) - 1
      j = a % col(ij)
      y_i_sum = y_i_sum + a % val(ij) * x(j)
    end do
    y(i) = y_i_sum
  end do

  end subroutine
