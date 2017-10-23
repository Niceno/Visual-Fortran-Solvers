!==============================================================================!
  subroutine Demo_LDLT_Solver_From_Tflows
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
  include "Compress_Matrix.int"               
  include "Create_Matrix_Compressed.int"               
  include "Create_Preconditioning_Matrix_Compressed.int"
!-----------------------------------[Locals]-----------------------------------!
  integer           :: n
  real, allocatable :: b(:), x(:), y(:), r(:)
  type(Matrix)      :: a_matrix, p_matrix
  real              :: error, time_ps, time_pe, time_ss, time_se
!==============================================================================!

  !------------------!
  !   Praparations   !
  !------------------!

  ! Create compressed system matrices
  call Create_Matrix_Compressed(a_matrix, NX, NY, NZ)
  n = a_matrix % n
  if(n<=64) call Print_Matrix_Compressed("Compressed a_matrix:", a_matrix)

  call Create_Preconditioning_Matrix_Compressed(p_matrix, a_matrix, 0)
  if(n<=64) call Print_Matrix_Compressed("Compressed p_matrix:", p_matrix)

  ! Finish memory allocation
  allocate (b(n))
  allocate (x(n))
  allocate (y(n))
  allocate (r(n))

  ! Fill the right hand side
  b = 0.1

  !------------------------!
  !   Actual computation   !
  !------------------------!

  ! Perform Cholesky factorization on the matrix to fin the lower one
  call Cpu_Time(time_ps)
  call Prec_Form(n, a_matrix, p_matrix)
  call Cpu_Time(time_pe)
  if(n<=64) call Print_Matrix_Compressed("p_matrix after factorization:", p_matrix)

  ! Compute x
  call Cpu_Time(time_ss)
  call Prec_Solve(n, -1, a_matrix, p_matrix, x, b) 
  call Cpu_Time(time_se)
  if(n<64) call Print_Vector("Solution x:", x) 

  write(*,*) '# Time for matrix preparation:', time_pe - time_ps
  write(*,*) '# Time for solution:          ', time_se - time_ss

  ! Check result
  call Matrix_Vector_Multiply_Compressed(y, a_matrix, x)
  if(n<64) call Print_Vector("Vector y should resemble source term:", y) 
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

  end subroutine Demo_LDLT_Solver_From_Tflows
