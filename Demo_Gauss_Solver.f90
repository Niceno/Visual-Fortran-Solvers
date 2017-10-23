!==============================================================================!
  subroutine Demo_Gauss_Solver
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Matrix_Mod
  use Globals_Mod
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Interfaces]---------------------------------!
  include "Input_Output/Input_Output.int"
  include "Linear_Algebra/Linear_Algebra.int"
  include "Linear_Solvers/Linear_Solvers.int"
  include "Create_Matrix_Compressed.int"               
  include "Expand_Matrix.int"
!-----------------------------------[Locals]-----------------------------------!
  integer           :: n
  real, allocatable :: a_matrix(:,:), g_matrix(:,:)
  real, allocatable :: b(:), b_o(:), x(:), y(:), r(:)
  real              :: error, time_ps, time_pe, time_ss, time_se
  type(Matrix)      :: c_matrix
!==============================================================================!

  !------------------!
  !   Praparations   !
  !------------------!

  ! Create compressed system matrix
  call Create_Matrix_Compressed(c_matrix, NX, NY, NZ)
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

  !------------------------!
  !   Actual computation   !
  !------------------------!

  ! Perform Gaussian elimination on matrix and r.h.s. vector
  b_o = b  ! store original "b" vector
  call Cpu_Time(time_ps)
  call Gaussian_Elimination(g_matrix, b, a_matrix)
  call Cpu_Time(time_pe)
  if(n<=64) call Print_Matrix("g_matrix after elimination:", g_matrix)
  if(n<=64) call Print_Vector("vector b after elimination:", b)

  ! Perform backward substitution
  call Cpu_Time(time_ss)
  call Backward_Substitution(x, g_matrix, b)
  call Cpu_Time(time_se)
  if(n<64) call Print_Vector("Solution x after backward substitution:", x) 

  write(*,*) '# Time for matrix preparation:', time_pe - time_ps
  write(*,*) '# Time for solution:          ', time_se - time_ss

  !------------------------!
  !   Check the solution   !
  !------------------------!
  call Matrix_Vector_Multiply(y, a_matrix, x)
  if(n<64) call Print_Vector("Vector y should recover the source term:", y)
  r = b_o - y
  call Vector_Vector_Dot_Product(error, r, r)
  write(*,*) "Error: ", sqrt(error)  

  !-------------------------!
  !   Clean-up the memory   !
  !-------------------------!
  deallocate(r)
  deallocate(y)
  deallocate(x)
  deallocate(b_o)
  deallocate(b)
  deallocate(g_matrix)
  deallocate(a_matrix)
  call deallocate_Matrix(c_matrix)

  end subroutine Demo_Gauss_Solver
