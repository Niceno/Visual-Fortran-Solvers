!==============================================================================!
  subroutine Demo_Cholesky_Solver
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Interfaces]---------------------------------!
  include "Backward_Substitution.int"
  include "Forward_Substitution.int"
  include "Print_Matrix.int"               
  include "Print_Vector.int"               
  include "Cholesky_Factorization.int"
  include "Load_Linear_System.int"
  include "Matrix_Vector_Multiply.int"
  include "Vector_Vector_Dot_Product.int"
!-----------------------------------[Locals]-----------------------------------!
  integer           :: n
  real, allocatable :: a_matrix(:,:), p_matrix(:,:)
  real, allocatable :: b(:), x(:), y(:), r(:)
  real              :: error
!==============================================================================!

  ! Read the system from the file system
  call Load_Linear_System(n, a_matrix, b)

  ! Finish memory allocation
  allocate (p_matrix(n,n))
  allocate (x(n))
  allocate (y(n))
  allocate (r(n))

  ! Just print original matrix
  call Print_Matrix("a_matrix:", a_matrix)

  ! Perform Cholesky factorization on the matrix to fin the lower one
  call Cholesky_Factorization(p_matrix, a_matrix)
  call Print_Matrix("p_matrix after Cholesky factorization", p_matrix)

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
