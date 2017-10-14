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
  include "Matrix_Vector_Multiply.int"
  include "Matrix_Vector_Multiply_Compressed.int"
  include "Vector_Vector_Dot_Product.int"
!-----------------------------------[Locals]-----------------------------------!
  integer :: row, col, choice, i
!==============================================================================!

  ! Full matrix
  integer, parameter    :: n = 10
  real, dimension(n, n) :: o_matrix  ! original matrix
  real, dimension(n)    :: b, x, y, r
  data (o_matrix( 1,col), col=1,n) / 44, -4, -3,  0, -1,  0,  0,  0,  0,  0 /
  data (o_matrix( 2,col), col=1,n) / -4, 44, -4, -3,  0, -1,  0,  0,  0,  0 /
  data (o_matrix( 3,col), col=1,n) / -3, -4, 44, -4, -3,  0, -1,  0,  0,  0 /
  data (o_matrix( 4,col), col=1,n) /  0, -3, -4, 44, -4, -3,  0, -1,  0,  0 /
  data (o_matrix( 5,col), col=1,n) / -1,  0, -3, -4, 44, -4, -3,  0, -1,  0 /
  data (o_matrix( 6,col), col=1,n) /  0, -1,  0, -3, -4, 44, -4, -3,  0, -1 /
  data (o_matrix( 7,col), col=1,n) /  0,  0, -1,  0, -3, -4, 44, -4, -3,  0 /
  data (o_matrix( 8,col), col=1,n) /  0,  0,  0, -1,  0, -3, -4, 44, -4, -3 /
  data (o_matrix( 9,col), col=1,n) /  0,  0,  0,  0, -1,  0, -3, -4, 44, -4 /
  data (o_matrix(10,col), col=1,n) /  0,  0,  0,  0,  0, -1,  0, -3, -4, 44 /
  data (b(row), row=1,n)           /  1,  2,  3,  4,  5,  5,  4,  3,  2,  1 /

  ! Compressed matrix
  type(Matrix) :: a_matrix, p_matrix
  real         :: error           
!------------------------------------------------------------------------------!

  call Print_Matrix("Original matrix o_matrix", o_matrix)

  ! Compress matrix "o_matrix" and store it in "a_matrix"
  call Compress_Matrix(a_matrix, o_matrix)

  ! Do the same with "p_matrix", just to allocate memory, really
  call Compress_Matrix(p_matrix, o_matrix)
  p_matrix % val = 0

  call Print_Matrix_Compressed("Compressed a_matrix:", a_matrix)

  ! Perform Cholesky factorization on the matrix to fin the lower one
  call Cholesky_Factorization_Compressed(p_matrix, a_matrix)
  call Print_Matrix_Compressed("p_matrix after factorization:", p_matrix)

  ! Compute y by forward substitution
  call Forward_Substitution_Compressed(y, p_matrix, b)
  call Print_Vector("Vector y after forward substitution:", y) 

  ! Compute x by backward substitution
  call Backward_Substitution_Compressed(x, p_matrix, y)
  call Print_Vector("Solution x after backward substitution:", x) 

  ! Check result
  call Matrix_Vector_Multiply_Compressed(y, a_matrix, x)
  call Print_Vector("Vector y should resemble source term:", y) 
  r = b - y
  call Vector_Vector_Dot_Product(error, r, r)
  write(*,*) "Error: ", sqrt(error)

  end subroutine Demo_Incomplete_Cholesky_Solver
