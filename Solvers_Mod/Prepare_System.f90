!==============================================================================!
  subroutine Solvers_Mod_Prepare_System(grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type) :: grid
!-----------------------------------[Locals]-----------------------------------!
  real :: time_ps, time_pe, time_ss, time_se
!==============================================================================!

  ! Create compressed system matrix
  call Matrix_Mod_Create_Compressed(a_sparse, grid)
  call In_Out_Mod_Print_Matrix_Compressed("Compressed a_sparse:", a_sparse)

  ! Finish memory allocation
  call Solvers_Mod_Allocate_Vectors(a_sparse % n)

  ! Fill the right hand side and store its original value
  b  (:) = 0.1
  b_o(:) = b(:)  ! original value

  end subroutine
