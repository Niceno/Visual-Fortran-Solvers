!==============================================================================!
  subroutine Demo_Mod_Fill_In(fill_in, grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer         :: fill_in
  type(Grid_Type) :: grid
!-----------------------------------[Locals]-----------------------------------!
  type(Sparse_Type) :: a  ! original matrix
  type(Sparse_Type) :: c  ! preconditioning matrix
!==============================================================================!

  call Sparse_Mod_Create(a, grid)

  call In_Out_Mod_Print_Sparse("Compressed a:", a)

  call Sparse_Mod_Create_Preconditioning(c, a, fill_in)

  call In_Out_Mod_Print_Sparse("Compressed a:", c)

  end subroutine
