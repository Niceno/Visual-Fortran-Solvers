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
  include "Vector_Vector_Dot_Product.int"
!-----------------------------------[Locals]-----------------------------------!
  integer :: row, col, choice, i
  real    :: error
!==============================================================================!

  ! Matrix for Gaussian elimination 
  integer, parameter      :: n = 4
  real, dimension(n, n) :: matrix_a, matrix_g
  real, dimension(n)     :: b, b_o, x, y, r
  data (matrix_a(1,col), col=1,n) /  1.0,  2.0,  1.0, -1.0 /  ! =--> row 1
  data (matrix_a(2,col), col=1,n) /  3.0,  2.0,  4.0,  4.0 /  ! =--> row 2
  data (matrix_a(3,col), col=1,n) /  4.0,  4.0,  3.0,  4.0 /  ! =--> row 3
  data (matrix_a(4,col), col=1,n) /  2.0,  0.0,  1.0,  5.0 /  ! =--> row 4
  data (b(row),   row=1,n)        /  5.0, 16.0, 22.0, 15.0 /

  ! Just print original matrix
  call Print_Matrix("matrix_a:", matrix_a)

  ! Perform gauissian elimination on matrix and r.h.s. vector
  b_o = b  ! store original "b" vector
  call Gaussian_Elimination(matrix_g, b, matrix_a)
  call Print_Matrix("matrix_g after elimination:", matrix_g)
  call Print_Vector("vector b after elimination:", b)

  ! Perform backward substitution
  call Backward_Substitution(x, matrix_g, b)
  call Print_Vector("Solution x after backward substitution:", x) 

  ! Multiply original matrix with solution vector to check result
  call Matrix_Vector_Multiply(y, matrix_a, x)
  call Print_Vector("Vector y should recover the source term:", y)
  r = b_o - y
  call Vector_Vector_Dot_Product(error, r, r)
  write(*,*) "Error: ", sqrt(error)  

  end subroutine Demo_Gauss_Solver
