!==============================================================================!
  subroutine Demo_Incomplete_Cholesky_Solver
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Matrix_Mod
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Interfaces]---------------------------------!
  include "Print_Matrix.int"               
  include "Print_Matrix_Compressed.int"               
  include "Print_Vector.int"               
  include "Cholesky_Factorization.int"
  include "Cholesky_Factorization_Compressed.int"
  include "Compress_Matrix.int"               
  include "Forward_Substitution_Compressed.int"
  include "Backward_Substitution_Compressed.int"
  include "Expand_Matrix.int"               
  include "Matrix_Vector_Multiply_Compressed.int"
!-----------------------------------[Locals]-----------------------------------!
  integer :: row, col, choice, i
!==============================================================================!

  ! Full matrix
  integer, parameter    :: n = 10
  real, dimension(n, n) :: f_matrix
  real, dimension(n)    :: b, x, y, r
  data (f_matrix( 1,col), col=1,n) / 44, -4, -3,  0, -1,  0,  0,  0,  0,  0 /
  data (f_matrix( 2,col), col=1,n) / -4, 44, -4, -3,  0, -1,  0,  0,  0,  0 /
  data (f_matrix( 3,col), col=1,n) / -3, -4, 44, -4, -3,  0, -1,  0,  0,  0 /
  data (f_matrix( 4,col), col=1,n) /  0, -3, -4, 44, -4, -3,  0, -1,  0,  0 /
  data (f_matrix( 5,col), col=1,n) / -1,  0, -3, -4, 44, -4, -3,  0, -1,  0 /
  data (f_matrix( 6,col), col=1,n) /  0, -1,  0, -3, -4, 44, -4, -3,  0, -1 /
  data (f_matrix( 7,col), col=1,n) /  0,  0, -1,  0, -3, -4, 44, -4, -3,  0 /
  data (f_matrix( 8,col), col=1,n) /  0,  0,  0, -1,  0, -3, -4, 44, -4, -3 /
  data (f_matrix( 9,col), col=1,n) /  0,  0,  0,  0, -1,  0, -3, -4, 44, -4 /
  data (f_matrix(10,col), col=1,n) /  0,  0,  0,  0,  0, -1,  0, -3, -4, 44 /
  data (b(row), row=1,n)           /  1,  2,  3,  4,  5,  5,  4,  3,  2,  1 /

  ! Compressed matrix
  type(Matrix) :: c_matrix
  real         :: error           
!------------------------------------------------------------------------------!

  call Print_Matrix("Original matrix f_matrix", f_matrix)

  ! Compress matrix "f_matrix" and store it in "c_matrix"
  call Compress_Matrix(c_matrix, f_matrix)

  call Print_Matrix_Compressed("Compressed c_matrix:", c_matrix)

  ! Perform Cholesky factorization on the matrix to fin the lower one
  call Cholesky_Factorization_Compressed(c_matrix, f_matrix)
  call Print_Matrix_Compressed("c_matrix after factorization:", c_matrix)

  ! Compute y by forward substitution
  call Forward_Substitution_Compressed(y, c_matrix, b)
  call Print_Vector("Vector y after forward substitution:", y) 

  ! Compute x by backward substitution
  call Backward_Substitution_Compressed(x, c_matrix, y)
  call Print_Vector("Vector x after forward substitution:", x) 

  ! Check result
  call Matrix_Vector_Multiply(y, f_matrix, x)
  ! later: call Matrix_Vector_Multiply_Compressed(y, c_matrix, x)
  r = b - y
  call Vector_Vector_Dot_Product(error, r, r)
  write(*,*) "Error: ", error  

  end subroutine Demo_Incomplete_Cholesky_Solver
