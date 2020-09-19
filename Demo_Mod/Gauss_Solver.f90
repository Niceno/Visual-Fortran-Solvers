!==============================================================================!
  subroutine Demo_Mod_Gauss_Solver(grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type) :: grid
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
  call Matrix_Mod_Create_Compressed(c_matrix, grid)
  call In_Out_Mod_Print_Matrix_Compressed("Compressed c_matrix:", c_matrix)

  ! Create two full matrices from the compressed one
  call Matrix_Mod_Expand(a_matrix, c_matrix)
  call Matrix_Mod_Expand(g_matrix, c_matrix)
  g_matrix = 0

  ! Finish memory allocation
  n = c_matrix % n
  allocate (b(n))
  allocate (b_o(n))
  allocate (x  (n))
  allocate (y  (n))
  allocate (r  (n))

  ! Fill the right hand side
  b = 0.1

  ! Just print original matrix
  call In_Out_Mod_Print_Matrix("a_matrix:", a_matrix)

  !------------------------!
  !   Actual computation   !
  !------------------------!

  ! Perform Gaussian elimination on matrix and r.h.s. vector
  b_o = b  ! store original "b" vector
  call Cpu_Time(time_ps)
  call Solvers_Mod_Gaussian_Elimination(g_matrix, b, a_matrix)
  call Cpu_Time(time_pe)
  call In_Out_Mod_Print_Matrix("g_matrix after elimination:", g_matrix)
  call In_Out_Mod_Print_Vector("vector b after elimination:", b)

  ! Perform backward substitution
  call Cpu_Time(time_ss)
  call Solvers_Mod_Backward_Substitution(x, g_matrix, b)
  call Cpu_Time(time_se)
  call In_Out_Mod_Print_Vector("Solution x after backward substitution:", x)

  write(*,*) '# Time for matrix preparation:', time_pe - time_ps
  write(*,*) '# Time for solution:          ', time_se - time_ss

  !------------------------!
  !   Check the solution   !
  !------------------------!
  call Lin_Alg_Mod_Matrix_Vector_Multiply(y, a_matrix, x)
  call In_Out_Mod_Print_Vector("Vector y should recover the source term:", y)
  r = b_o - y
  call Lin_Alg_Mod_Vector_Vector_Dot_Product(error, r, r)
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
  call Matrix_Mod_Deallocate(c_matrix)

  end subroutine
