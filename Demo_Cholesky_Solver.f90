!==============================================================================!
  subroutine Demo_Cholesky_Solver
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Matrix_Mod
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Interfaces]---------------------------------!
  include "Backward_Substitution.int"
  include "Forward_Substitution.int"
  include "Print_Matrix.int"               
  include "Print_Matrix_Compressed.int"               
  include "Print_Vector.int"               
  include "Cholesky_Factorization.int"
  include "Create_Matrix_Compressed.int"               
  include "Load_Linear_System.int"
  include "Matrix_Vector_Multiply.int"
  include "Vector_Vector_Dot_Product.int"
  include "Expand_Matrix.int"
!-----------------------------------[Locals]-----------------------------------!
  integer           :: n
  real, allocatable :: a_matrix(:,:), p_matrix(:,:)
  real, allocatable :: b(:), x(:), y(:), r(:)
  type(Matrix)      :: c_matrix
  real              :: error
!==============================================================================!

  ! Create compressed system matrix
  call Create_Matrix_Compressed(c_matrix, 7, 7, 7, 0)
  n = c_matrix % n
  if(n<50) call Print_Matrix_Compressed("Compressed c_matrix:", c_matrix)

  ! Create two full matrices from the compressed one
  call Expand_Matrix(a_matrix, c_matrix)
  call Expand_Matrix(p_matrix, c_matrix)
  p_matrix = 0

  ! Finish memory allocation
  allocate (b(n))
  allocate (x(n))
  allocate (y(n))
  allocate (r(n))

  ! Fill the right hand side
  b = 0.1

  ! Just print original matrix
  if(n<50) call Print_Matrix("a_matrix:", a_matrix)

  ! Perform Cholesky factorization on the matrix to fin the lower one
  call Cholesky_Factorization(p_matrix, a_matrix)
  if(n<50) call Print_Matrix("p_matrix after Cholesky factorization", p_matrix)

  ! Compute y by forward substitution
  call Forward_Substitution(y, p_matrix, b)
  call Print_Vector("Vector y after forward substitution:", y) 

  ! Compute x by backward substitution
  call Backward_Substitution(x, p_matrix, y)
  call Print_Vector("Solution x after backward substitution:", x) 

  ! Check result
  call Matrix_Vector_Multiply(y, a_matrix, x)
  call Print_Vector("Vector y should recover the source term:", y) 
  r = b - y
  call Vector_Vector_Dot_Product(error, r, r)
  write(*,*) "Error: ", sqrt(error)  

  end subroutine Demo_Cholesky_Solver
