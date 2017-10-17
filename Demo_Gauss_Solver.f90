!==============================================================================!
  subroutine Demo_Gauss_Solver
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Matrix_Mod
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Interfaces]---------------------------------!
  include "Create_Matrix_Compressed.int"               
  include "Expand_Matrix.int"
  include "Print_Matrix.int"               
  include "Gaussian_Elimination.int"
  include "Print_Vector.int"               
  include "Matrix_Vector_Multiply.int"
  include "Vector_Vector_Dot_Product.int"
  include "Backward_Substitution.int"
!-----------------------------------[Locals]-----------------------------------!
  integer           :: n
  real, allocatable :: a_matrix(:,:), g_matrix(:,:)
  real, allocatable :: b(:), b_o(:), x(:), y(:), r(:)
  real              :: error
  type(Matrix)      :: c_matrix
!==============================================================================!

  ! Create compressed system matrix
  call Create_Matrix_Compressed(c_matrix, 16, 16, 16, 0)
  n = c_matrix % n
  if(n<=64) call Print_Matrix_Compressed("Compressed c_matrix:", c_matrix)

  ! Create two full matrices from the compressed one
  call Expand_Matrix(a_matrix, c_matrix)
  call Expand_Matrix(g_matrix, c_matrix)
  g_matrix = 0

  ! Finish memory allocation
  allocate (b(n))
  allocate (b_o(n))
  allocate (x  (n))
  allocate (y  (n))
  allocate (r  (n))

  ! Fill the right hand side
  b = 0.1

  ! Just print original matrix
  if(n<=64) call Print_Matrix("a_matrix:", a_matrix)

  ! Perform gauissian elimination on matrix and r.h.s. vector
  b_o = b  ! store original "b" vector
  call Gaussian_Elimination(g_matrix, b, a_matrix)
  if(n<=64) call Print_Matrix("g_matrix after elimination:", g_matrix)
  if(n<=64) call Print_Vector("vector b after elimination:", b)

  ! Perform backward substitution
  call Backward_Substitution(x, g_matrix, b)
  call Print_Vector("Solution x after backward substitution:", x) 

  ! Multiply original matrix with solution vector to check result
  call Matrix_Vector_Multiply(y, a_matrix, x)
  call Print_Vector("Vector y should recover the source term:", y)
  r = b_o - y
  call Vector_Vector_Dot_Product(error, r, r)
  write(*,*) "Error: ", sqrt(error)  

  end subroutine Demo_Gauss_Solver
