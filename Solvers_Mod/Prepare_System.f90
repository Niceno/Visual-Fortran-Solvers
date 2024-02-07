!==============================================================================!
  subroutine Solvers_Mod_Prepare_System(grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type) :: grid
!==============================================================================!

  ! Finish memory allocation
  call Solvers_Mod_Allocate_Vectors(grid % nx * grid % ny * grid % nz)

  end subroutine
