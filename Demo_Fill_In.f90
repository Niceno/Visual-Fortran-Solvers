!==============================================================================!
  subroutine Demo_Fill_In(fill_in)
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Matrix_Mod
  use Constants_Mod
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer :: fill_in
!---------------------------------[Interfaces]---------------------------------!
  include "Input_Output/Input_Output.int"
  include "Linear_Algebra/Linear_Algebra.int"
  include "Compress_Matrix.int"               
  include "Expand_Matrix.int"               
  include "Create_Preconditioning_Matrix_Compressed.int"
!-----------------------------------[Locals]-----------------------------------!
  type(Matrix) :: a_matrix  ! original matrix
  type(Matrix) :: c_matrix  ! preconditioning matrix
  integer      :: n
!==============================================================================!

  call Create_Matrix_Compressed(a_matrix, NX, NY, NZ, 0)
  n = a_matrix % n
  if(n<=64) call Print_Matrix_Compressed("Compressed a_matrix:", a_matrix)

  call Create_Preconditioning_Matrix_Compressed(c_matrix, a_matrix, fill_in)

  if(n<=64) call Print_Matrix_Compressed("Compressed a_matrix:", c_matrix)

  end subroutine Demo_Fill_In
