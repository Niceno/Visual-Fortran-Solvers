!==============================================================================!
  subroutine Demo_LDLT_Solver
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Matrix_Mod
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Interfaces]---------------------------------!
  include "Print_Matrix.int"               
  include "Print_Matrix_Compressed.int"               
  include "Print_Vector.int"               
  include "Create_Matrix_Compressed.int"               
  include "LDLT_Factorization.int"  
  include "LDLT_Solution.int"
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
  call Create_Matrix_Compressed(c_matrix, 10, 10, 10, 0)
  n = c_matrix % n
  if(n<=64) call Print_Matrix_Compressed("Compressed c_matrix:", c_matrix)
 
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
  if(n<=64) call Print_Matrix("a_matrix:", a_matrix)

  ! Perform LDLT factorization on the matrix to fin the lower one
  call LDLT_Factorization(p_matrix, a_matrix)
  if(n<=64) call Print_Matrix("p_matrix after Cholesky factorization", p_matrix)

  ! Compute x
  call LDLT_Solution(x, p_matrix, b)
  call Print_Vector("Solution x:", x) 

  ! Check result
  call Matrix_Vector_Multiply(y, a_matrix, x)
  call Print_Vector("Vector y should recover the source term:", y) 
  r = b - y
  call Vector_Vector_Dot_Product(error, r, r)
  write(*,*) "Error: ", sqrt(error)  

  ! Free memory
  deallocate(a_matrix)
  deallocate(p_matrix)
  deallocate(b)
  deallocate(x)
  deallocate(y)
  deallocate(r)
  call deallocate_Matrix(c_matrix)

  end subroutine Demo_LDLT_Solver
