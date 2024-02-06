!==============================================================================!
  subroutine Solvers_Mod_Incomplete_Ldlt_From_Tflows(grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type) :: grid
!-----------------------------------[Locals]-----------------------------------!
  real :: time_ps, time_pe, time_ss, time_se
!==============================================================================!

  !------------------!
  !   Praparations   !
  !------------------!
  call Solvers_Mod_Prepare_System(grid)
  call P_Sparse % Sparse_Create_Preconditioning(a_sparse, 0)

  !------------------------!
  !   Actual computation   !
  !------------------------!

  ! Perform LDL^T factorization on the matrix to find the lower one
  call Cpu_Time(time_ps)
  call Solvers_Mod_Ldlt_Factorization_From_Tflows(p_sparse, a_sparse)
  call Cpu_Time(time_pe)
  call In_Out_Mod_Print_Sparse("p_sparse after factorization:", p_sparse)

  ! Compute x
  call Cpu_Time(time_ss)
  call Solvers_Mod_Ldlt_Solution_From_Tflows(a_sparse % n, -1,  &
                                             a_sparse, p_sparse, x, b)
  call Cpu_Time(time_se)
  !@ call In_Out_Mod_Print_Vector("Solution x:", x)

  print '(a,1es10.4)', ' # Time for matrix preparation: ', time_pe - time_ps
  print '(a,1es10.4)', ' # Time for solution:           ', time_se - time_ss

  !------------------------!
  !   Check the solution   !
  !------------------------!
  call Solvers_Mod_Check_Solution_Sparse(a_sparse)

  !-------------------------!
  !   Clean-up the memory   !
  !-------------------------!
  call Solvers_Mod_Deallocate()

  end subroutine
