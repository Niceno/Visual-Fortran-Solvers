!==============================================================================!
  subroutine Solvers_Mod_Check_Solution(full, sparse)
!------------------------------------------------------------------------------!
!   Check the solution                                                         !
!------------------------------------------------------------------------------!
  implicit none
  real, allocatable, optional :: full(:,:)
  type(Matrix_Type), optional :: sparse
!-----------------------------------[Locals]-----------------------------------!
  real :: error
!==============================================================================!


  if(present(full)) then
    call Lin_Alg_Mod_Matrix_Vector_Multiply(y, full, x)
  else if(present(sparse)) then
    !$acc enter data copyin(y)
    call Lin_Alg_Mod_Matrix_Vector_Multiply_Compressed(y, sparse, x)
    !$acc update self(y)
  end if

  r = b_o - y
  !$acc enter data copyin(error)
  call Lin_Alg_Mod_Vector_Vector_Dot_Product(error, r, r)
  !$acc update self(error)
  print '(a,1es10.4)', " # Error:                       ", sqrt(error)

  end subroutine
