!==============================================================================!
  subroutine Demo_Mod_Fill_In(fill_in, Grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer         :: fill_in
  type(Grid_Type) :: Grid
!-----------------------------------[Locals]-----------------------------------!
  type(Sparse_Type) :: A  ! original matrix
  type(Sparse_Type) :: C  ! preconditioning matrix
!==============================================================================!

  call A % Sparse_Create(Grid, singular=.false.)

  call IO % Plot_Sparse ("compressed_a",  A)
  call IO % Print_Sparse("Compressed A:", A)

  call C % Sparse_Create_Preconditioning(A, fill_in)

  call IO % Plot_Sparse ("preconditioned_c",  C)
  call IO % Print_Sparse("Preconditioned C:", C)

  end subroutine
