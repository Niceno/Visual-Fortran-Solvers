!==============================================================================!
  subroutine Lin_Alg_Mod_Sparse_X_Vector(y, A, x)
!------------------------------------------------------------------------------!
!>  Computes matrix vector product with matrix in compressed row format.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:) :: y
  type(Sparse_Type)  :: A
  real, dimension(:) :: x
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, ij
  real    :: sum
!==============================================================================!

  do i = 1, A % n
    sum = 0.0
    do ij = A % row(i), A % row(i+1) - 1
      j = A % col(ij)
      sum = sum + A % val(ij) * x(j)
    end do
    y(i) = sum
  end do

  end subroutine
