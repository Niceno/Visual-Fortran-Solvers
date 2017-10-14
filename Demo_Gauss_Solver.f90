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
  integer           :: n = 10
  real, allocatable :: matrix_a(:,:), matrix_g(:,:)
  real, allocatable :: b(:), b_o(:), x(:), y(:), r(:)

  ! Read the system from the file system
  open(9, file="A_b.dat")
  read(9, *) n
  allocate (matrix_a(n,n))
  do row=1,n
    read(9, *) (matrix_a(row,col), col=1,n)
  end do
  allocate (b(n))
  read(9, *) (b(row), row=1,n)
  close(9)

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
