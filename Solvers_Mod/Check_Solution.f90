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
    call Lin_Alg_Mod_Matrix_Vector_Multiply_Compressed(y, sparse, x)
  end if

  call In_Out_Mod_Print_Vector("Vector y should recover the source term:", y)
  r = b_o - y
  call Lin_Alg_Mod_Vector_Vector_Dot_Product(error, r, r)
  write(*,*) "# Error: ", sqrt(error)

  end subroutine
