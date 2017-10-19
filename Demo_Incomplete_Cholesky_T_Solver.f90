!==============================================================================!
  subroutine Demo_Incomplete_Cholesky_T_Solver
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Matrix_Mod
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer :: fill_in
!---------------------------------[Interfaces]---------------------------------!
  include "Print_Matrix.int"               
  include "Print_Matrix_Compressed.int"               
  include "Print_Vector.int"               
  include "Prec_Form.int"                 
  include "Prec_Solve.int"                 
  include "Cholesky_Factorization_Compressed.int"
  include "Compress_Matrix.int"               
  include "Create_Matrix_Compressed.int"               
  include "Forward_Substitution_Compressed.int"
  include "Backward_Substitution_Compressed.int"
  include "Expand_Matrix.int"               
  include "Load_Linear_System.int"
  include "Matrix_Vector_Multiply.int"
  include "Matrix_Vector_Multiply_Compressed.int"
  include "Vector_Vector_Dot_Product.int"
!-----------------------------------[Locals]-----------------------------------!
  integer           :: n
  real, allocatable :: b(:), x(:), y(:), r(:)
  type(Matrix)      :: a_matrix, p_matrix
  real              :: error           
!==============================================================================!

  ! Create compressed system matrices
  call Create_Matrix_Compressed(a_matrix,  3,  3,  3, 0)
  call Create_Matrix_Compressed(p_matrix,  3,  3,  3, 0)
  n = a_matrix % n
  if(n<=64) call Print_Matrix_Compressed("Compressed a_matrix:", a_matrix)

  p_matrix % val = 0
  ! if(n<=64) call Print_Matrix_Compressed("Compressed p_matrix:", p_matrix)

  ! Finish memory allocation
  allocate (b(n))
  allocate (x(n))
  allocate (y(n))
  allocate (r(n))

  ! Fill the right hand side
  b = 0.1

  ! Perform Cholesky factorization on the matrix to fin the lower one
  call Prec_Form(n, a_matrix, p_matrix)
  if(n<=64) call Print_Matrix_Compressed("p_matrix after factorization:", p_matrix)

  ! Compute x
  call Prec_Solve(n, -1, a_matrix, p_matrix, x, b) 
  call Print_Vector("Solution x:", x) 

  ! Check result
  call Matrix_Vector_Multiply_Compressed(y, a_matrix, x)
  call Print_Vector("Vector y should resemble source term:", y) 
  r = b - y
  call Vector_Vector_Dot_Product(error, r, r)
  write(*,*) "Error: ", sqrt(error)

  ! Free memory
  deallocate(b)
  deallocate(x)
  deallocate(y)
  deallocate(r)
  call deallocate_Matrix(a_matrix)
  call deallocate_Matrix(p_matrix)

  end subroutine Demo_Incomplete_Cholesky_T_Solver
