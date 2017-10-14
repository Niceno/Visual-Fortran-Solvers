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
  include "Load_Linear_System.int"
  include "Matrix_Vector_Multiply.int"
  include "Matrix_Vector_Multiply_Compressed.int"
  include "Vector_Vector_Dot_Product.int"
!-----------------------------------[Locals]-----------------------------------!
  integer           :: n
  real, allocatable :: o_matrix(:,:)
  real, allocatable :: b(:), x(:), y(:), r(:)
  type(Matrix)      :: a_matrix, p_matrix
  real              :: error           
!==============================================================================!

  ! Read the system from the file system
  call Load_Linear_System(n, o_matrix, b)

  ! Finish memory allocation
  allocate (x(n))
  allocate (y(n))
  allocate (r(n))

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
