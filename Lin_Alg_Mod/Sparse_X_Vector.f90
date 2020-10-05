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
  integer :: i, j, i_j, n
  real    :: y_i_sum
!==============================================================================!

  n = a % n  ! some checks would be possible

  !$acc  parallel loop        &
  !$acc& present(y)           &
  !$acc& present(a)           &
  !$acc& present(a % row(:))  &
  !$acc& present(a % col(:))  &
  !$acc& present(a % val(:))  &
  !$acc& present(x)           &
  !$acc& gang worker vector_length(32) num_workers(32)
  do i = 1, n
    y_i_sum = 0.0
    !$acc loop vector reduction(+:y_i_sum)
    do i_j = a % row(i), a % row(i+1) - 1
      j = a % col(i_j)
      y_i_sum = y_i_sum + a % val(i_j) * x(j)
    end do
    y(i) = y_i_sum
  end do

  end subroutine
