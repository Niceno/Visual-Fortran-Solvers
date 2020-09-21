!==============================================================================!
  subroutine Solvers_Mod_Incomplete_Cholesky(grid, fill_in)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type) :: grid
  integer         :: fill_in
!-----------------------------------[Locals]-----------------------------------!
  real :: time_ps, time_pe, time_ss, time_se
!==============================================================================!

  print *, '#=========================================================='
  print *, '# Solving the sytem with incomplete Cholesky decomposition'
  print *, '#----------------------------------------------------------'

  !------------------!
  !   Praparations   !
  !------------------!
  call Solvers_Mod_Prepare_System(grid)
  call Matrix_Mod_Create_Preconditioning_Compressed(p_sparse, a_sparse, fill_in)

  !------------------------!
  !   Actual computation   !
  !------------------------!

  ! Perform Cholesky factorization on the matrix to fin the lower one
  call Cpu_Time(time_ps)
  call Solvers_Mod_Cholesky_Factorization_Compressed(p_sparse, a_sparse)
  call Cpu_Time(time_pe)
  call In_Out_Mod_Print_Matrix_Compressed(  &
       "p_sparse after factorization:", p_sparse)

  ! Compute y by forward substitution
  call Cpu_Time(time_ss)
  call Solvers_Mod_Forward_Substitution_Compressed(y, p_sparse, b)

  ! Compute x by backward substitution
  call Solvers_Mod_Backward_Substitution_Compressed(x, p_sparse, y)
  call Cpu_Time(time_se)
  !@ call In_Out_Mod_Print_Vector("Solution x:", x)

  print '(a,1es10.4)', ' # Time for matrix preparation: ', time_pe - time_ps
  print '(a,1es10.4)', ' # Time for solution:           ', time_se - time_ss

  !------------------------!
  !   Check the solution   !
  !------------------------!
  call Solvers_Mod_Check_Solution(sparse = a_sparse)

  !-------------------------!
  !   Clean-up the memory   !
  !-------------------------!
  call Solvers_Mod_Deallocate()

  end subroutine
