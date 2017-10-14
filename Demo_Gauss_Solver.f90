!==============================================================================!
  subroutine Demo_Gauss_Solver
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Interfaces]---------------------------------!
  include "Backward_Substitution.int"
  include "Print_Matrix.int"               
  include "Print_Vector.int"               
  include "Gaussian_Elimination.int"
  include "Matrix_Vector_Multiply.int"
!-----------------------------------[Locals]-----------------------------------!
  integer :: row, col, choice, i
!==============================================================================!

  ! Matrix for Gaussian elimination 
  integer, parameter      :: n1 = 4
  real, dimension(n1, n1) :: matrix_a, matrix_g
  real, dimension(n1)     :: b, x, y1
  data (matrix_a(1,col), col=1,n1) /  1.0,  2.0,  1.0, -1.0 /  ! =--> row 1
  data (matrix_a(2,col), col=1,n1) /  3.0,  2.0,  4.0,  4.0 /  ! =--> row 2
  data (matrix_a(3,col), col=1,n1) /  4.0,  4.0,  3.0,  4.0 /  ! =--> row 3
  data (matrix_a(4,col), col=1,n1) /  2.0,  0.0,  1.0,  5.0 /  ! =--> row 4
  data (b(row),   row=1,n1) /  5.0, 16.0, 22.0, 15.0 /

  ! Just print original matrix
  call Print_Matrix("matrix_a:", matrix_a)

  ! Perform gauissian elimination on matrix and r.h.s. vector
  call Gaussian_Elimination(matrix_g, b, matrix_a)
  call Print_Matrix("matrix_g after elimination:", matrix_g)
  call Print_Vector("vector b after elimination:", b)

  ! Perform backward substitution
  call Backward_Substitution(x, matrix_g, b)
  call Print_Vector("solution vector x: ", x)

  ! Multiply original matrix with solution vector to check result
  call Matrix_Vector_Multiply(y1, matrix_a, x)
  call Print_Vector("Vector y1 should recover the source term:", y1)

  end subroutine Demo_Gauss_Solver
