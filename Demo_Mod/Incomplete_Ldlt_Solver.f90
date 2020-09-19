!==============================================================================!
  subroutine Demo_Mod_Incomplete_Ldlt_Solver(fill_in, grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type) :: grid
  integer         :: fill_in
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
  call Matrix_Mod_Create_Compressed(a_matrix, grid)
  call In_Out_Mod_Print_Matrix_Compressed("Compressed a_matrix:", a_matrix)

  call Matrix_Mod_Create_Preconditioning_Compressed(p_matrix, a_matrix, fill_in)
  call In_Out_Mod_Print_Matrix_Compressed("Compressed p_matrix:", p_matrix)

  ! Finish memory allocation
  n = a_matrix % n
  allocate (b(n))
  allocate (x(n))
  allocate (y(n))
  allocate (r(n))

  ! Fill the right hand side
  b = 0.1

  !------------------------!
  !   Actual computation   !
  !------------------------!

  ! Perform LDLT factorization on the matrix to fin the lower one
  call Cpu_Time(time_ps)
  call Solvers_Mod_Ldlt_Factorization_Compressed(p_matrix, a_matrix)
  call Cpu_Time(time_pe)
  call In_Out_Mod_Print_Matrix_Compressed(  &
       "p_matrix after factorization:", p_matrix)

  ! Compute x
  call Cpu_Time(time_ss)
  call Solvers_Mod_Ldlt_Solution_Compressed(x, p_matrix, b)
  call Cpu_Time(time_se)
  call In_Out_Mod_Print_Vector("Solution x:", x)

  write(*,*) '# Time for matrix preparation:', time_pe - time_ps
  write(*,*) '# Time for solution:          ', time_se - time_ss

  !------------------------!
  !   Check the solution   !
  !------------------------!
  call Lin_Alg_Mod_Matrix_Vector_Multiply_Compressed(y, a_matrix, x)
  call In_Out_Mod_Print_Vector("Vector y should resemble source term:", y)
  r = b - y
  call Lin_Alg_Mod_Vector_Vector_Dot_Product(error, r, r)
  write(*,*) "Error: ", sqrt(error)

  !-------------------------!
  !   Clean-up the memory   !
  !-------------------------!
  deallocate(b)
  deallocate(x)
  deallocate(y)
  deallocate(r)
  call Matrix_Mod_Deallocate(a_matrix)
  call Matrix_Mod_Deallocate(p_matrix)

  end subroutine
