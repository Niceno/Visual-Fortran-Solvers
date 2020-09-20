!==============================================================================!
  subroutine Solvers_Mod_Ldlt(grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type) :: grid
!-----------------------------------[Locals]-----------------------------------!
  real :: time_ps, time_pe, time_ss, time_se
!==============================================================================!

  print *, '#=========================================================='
  print *, '# Solving the sytem with LDL^T decomposition'
  print *, '#----------------------------------------------------------'

  !------------------!
  !   Praparations   !
  !------------------!
  call Solvers_Mod_Prepare_System(grid)

  ! Create two full matrices from the compressed one
  call Matrix_Mod_Expand(a_matrix, a_sparse)
  call Matrix_Mod_Expand(p_matrix, a_sparse)
  p_matrix = 0

  ! Just print original matrix
  call In_Out_Mod_Print_Matrix("a_matrix:", a_matrix)

  !------------------------!
  !   Actual computation   !
  !------------------------!

  ! Perform LDLT factorization on the matrix to fin the lower one
  call Cpu_Time(time_ps)
  call Solvers_Mod_Ldlt_Factorization(p_matrix, a_matrix)
  call Cpu_Time(time_pe)
  call In_Out_Mod_Print_Matrix(   &
       "p_matrix after Cholesky factorization", p_matrix)

  ! Compute x
  call Cpu_Time(time_ss)
  call Solvers_Mod_Ldlt_Solution(x, p_matrix, b)
  call Cpu_Time(time_se)
  call In_Out_Mod_Print_Vector("Solution x:", x)

  print '(a,1es10.4)', ' # Time for matrix preparation: ', time_pe - time_ps
  print '(a,1es10.4)', ' # Time for solution:           ', time_se - time_ss

  !------------------------!
  !   Check the solition   !
  !------------------------!
  call Solvers_Mod_Check_Solution(full = a_matrix)

  !-------------------------!
  !   Clean-up the memory   !
  !-------------------------!
  call Solvers_Mod_Deallocate()

  end subroutine
