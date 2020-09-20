!==============================================================================!
  subroutine Solvers_Mod_Gauss(grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type) :: grid
!-----------------------------------[Locals]-----------------------------------!
  real    :: time_ps, time_pe, time_ss, time_se
  integer :: bw
!==============================================================================!

  print *, '#=========================================================='
  print *, '# Solving the sytem with Gaussian elimination'
  print *, '#----------------------------------------------------------'

  !------------------!
  !   Praparations   !
  !------------------!
  call Solvers_Mod_Prepare_System(grid)

  ! Create two full matrices from the compressed one
  call Matrix_Mod_Expand(a_matrix, a_sparse, bw)
  call Matrix_Mod_Expand(p_matrix, a_sparse, bw)
  p_matrix = 0

  ! Just print original matrix
  call In_Out_Mod_Print_Matrix("a_matrix:", a_matrix)

  !------------------------!
  !   Actual computation   !
  !------------------------!

  ! Perform Gauss elimination on matrix and r.h.s. vector
  call Cpu_Time(time_ps)
  call Solvers_Mod_Gauss_Elimination(p_matrix, b, a_matrix, bw)
  call Cpu_Time(time_pe)
  call In_Out_Mod_Print_Matrix("p_matrix after elimination:", p_matrix)
  !@ call In_Out_Mod_Print_Vector("vector b after elimination:", b)

  ! Perform backward substitution
  call Cpu_Time(time_ss)
  call Solvers_Mod_Backward_Substitution(x, p_matrix, b)
  call Cpu_Time(time_se)
  !@ call In_Out_Mod_Print_Vector("Solution x after backward substitution:", x)

  print '(a,1es10.4)', ' # Time for matrix preparation: ', time_pe - time_ps
  print '(a,1es10.4)', ' # Time for solution:           ', time_se - time_ss

  !------------------------!
  !   Check the solution   !
  !------------------------!
  call Solvers_Mod_Check_Solution(full = a_matrix)

  !-------------------------!
  !   Clean-up the memory   !
  !-------------------------!
  call Solvers_Mod_Deallocate()

  end subroutine
