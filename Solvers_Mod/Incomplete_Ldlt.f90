!==============================================================================!
  subroutine Solvers_Mod_Incomplete_Ldlt(fill_in, grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type) :: grid
  integer         :: fill_in
!-----------------------------------[Locals]-----------------------------------!
  real :: time_ps, time_pe, time_ss, time_se
!==============================================================================!

  !------------------!
  !   Praparations   !
  !------------------!
  call Solvers_Mod_Prepare_System(grid)

  call Matrix_Mod_Create_Preconditioning_Compressed(p_sparse, a_sparse, fill_in)
  call In_Out_Mod_Print_Matrix_Compressed("Compressed p_sparse:", p_sparse)

  !------------------------!
  !   Actual computation   !
  !------------------------!

  ! Perform LDLT factorization on the matrix to fin the lower one
  call Cpu_Time(time_ps)
  call Solvers_Mod_Ldlt_Factorization_Compressed(p_sparse, a_sparse)
  call Cpu_Time(time_pe)
  call In_Out_Mod_Print_Matrix_Compressed(  &
       "p_sparse after factorization:", p_sparse)

  ! Compute x
  call Cpu_Time(time_ss)
  call Solvers_Mod_Ldlt_Solution_Compressed(x, p_sparse, b)
  call Cpu_Time(time_se)
  call In_Out_Mod_Print_Vector("Solution x:", x)

  write(*,*) '# Time for matrix preparation:', time_pe - time_ps
  write(*,*) '# Time for solution:          ', time_se - time_ss

  !------------------------!
  !   Check the solution   !
  !------------------------!
  call Solvers_Mod_Check_Solution(sparse = a_sparse)

  !-------------------------!
  !   Clean-up the memory   !
  !-------------------------!
  call Solvers_Mod_Deallocate()

  end subroutine
