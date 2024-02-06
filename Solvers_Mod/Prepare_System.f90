!==============================================================================!
  subroutine Solvers_Mod_Prepare_System(grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type) :: grid
!==============================================================================!

  ! Create sparse system matrix
  call A_Sparse % Sparse_Create(grid)
  call In_Out_Mod_Print_Sparse("Sparse A_Sparse:", A_Sparse)

  ! Finish memory allocation
  call Solvers_Mod_Allocate_Vectors(A_Sparse % n)

  ! Fill the right hand side and store its original value
  b  (:) = 0.1
  b_o(:) = b(:)  ! original value

  ! Set initial guess to zero, since it can influence iteratie solvers
  x(:) = 0.0

  end subroutine
