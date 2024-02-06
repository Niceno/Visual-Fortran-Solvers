!==============================================================================!
  subroutine Demo_Mod_Fill_In(fill_in, grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer         :: fill_in
  type(Grid_Type) :: grid
!-----------------------------------[Locals]-----------------------------------!
  type(Sparse_Type) :: A  ! original matrix
  type(Sparse_Type) :: C  ! preconditioning matrix
!==============================================================================!

  call A % Sparse_Create(grid)

  call In_Out_Mod_Print_Sparse("Compressed A:", A)

  call C % Sparse_Create_Preconditioning(A, fill_in)

  call In_Out_Mod_Print_Sparse("Compressed a:", C)

  end subroutine
