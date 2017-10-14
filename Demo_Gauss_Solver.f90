!==============================================================================!
  subroutine Demo_Gauss_Solver
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Interfaces]---------------------------------!
  include "Backward_Substitution.int"
  include "Print_Matrix.int"               
  include "Print_Vector.int"               
  include "Gaussian_Elimination.int"
  include "Load_Linear_System.int"
  include "Matrix_Vector_Multiply.int"
  include "Vector_Vector_Dot_Product.int"
!-----------------------------------[Locals]-----------------------------------!
  integer           :: n
  real, allocatable :: matrix_a(:,:), matrix_g(:,:)
  real, allocatable :: b(:), b_o(:), x(:), y(:), r(:)
  real              :: error
!==============================================================================!

  ! Read the system from the file system
  call Load_Linear_System(n, matrix_a, b)

  ! Finish memory allocation
  allocate (matrix_g(n,n))
  allocate (b_o(n))
  allocate (x  (n))
  allocate (y  (n))
  allocate (r  (n))

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
