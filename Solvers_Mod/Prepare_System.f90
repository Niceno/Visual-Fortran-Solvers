!==============================================================================!
  subroutine Solvers_Mod_Prepare_System(grid, b)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type) :: grid
  real            :: b(:)
!==============================================================================!

  ! Finish memory allocation
  call Solvers_Mod_Allocate_Vectors(grid % nx * grid % ny * grid % nz)

  ! Store the original value of the right hand side for checking
  b_o(:) = b(:)

  end subroutine
