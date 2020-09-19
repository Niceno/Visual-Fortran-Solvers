!==============================================================================!
  subroutine Demo_Mod_Fill_In(fill_in, grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer         :: fill_in
  type(Grid_Type) :: grid
!-----------------------------------[Locals]-----------------------------------!
  type(Matrix) :: a_matrix  ! original matrix
  type(Matrix) :: c_matrix  ! preconditioning matrix
!==============================================================================!

  call Matrix_Mod_Create_Compressed(a_matrix, grid)

  call In_Out_Mod_Print_Matrix_Compressed("Compressed a_matrix:", a_matrix)

  call Matrix_Mod_Create_Preconditioning_Compressed(c_matrix, a_matrix, fill_in)

  call In_Out_Mod_Print_Matrix_Compressed("Compressed a_matrix:", c_matrix)

  end subroutine
