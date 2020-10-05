!==============================================================================!
  subroutine Lin_Alg_Mod_Matrix_Vector_Multiply_Compressed(y, c, x)
!------------------------------------------------------------------------------!
!   Computes matrix vector product with matrix in compressed row format.       !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:) :: y
  type(Matrix_Type)  :: c
  real, dimension(:) :: x
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, i_j, n
  real    :: y_i_sum
!==============================================================================!

  n  = c % n  ! some checks would be possible

  !$acc  parallel loop        &
  !$acc& present(y)           &
  !$acc& present(c)           &
  !$acc& present(c % row(:))  &
  !$acc& present(c % col(:))  &
  !$acc& present(c % val(:))  &
  !$acc& present(x)           &
  !$acc& gang worker vector_length(32) num_workers(32)
  do i = 1, n
    y_i_sum = 0.0
    !$acc loop vector reduction(+:y_i_sum)
    do i_j = c % row(i), c % row(i+1) - 1
      j = c % col(i_j)
      y_i_sum = y_i_sum + c % val(i_j) * x(j)
    end do
    y(i) = y_i_sum
  end do

  end subroutine
