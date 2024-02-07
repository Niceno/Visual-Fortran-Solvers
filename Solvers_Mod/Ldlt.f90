!==============================================================================!
  subroutine Solvers_Mod_Ldlt(grid)
!------------------------------------------------------------------------------!
!   In a nutshell:                                                             !
!   1 - calls Ldlt Factorization                                               !
!   2 - calls Ldlt Solution                                                    !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type) :: grid  !! computational grid
!-----------------------------------[Locals]-----------------------------------!
  real                      :: time_ps, time_pe, time_ss, time_se
  type(Dense_Type), pointer :: A, LDL
!==============================================================================!

  print *, '#=========================================================='
  print *, '# Solving the sytem with LDL^T decomposition'
  print *, '#----------------------------------------------------------'

  A   => a_square
  LDL => p_square

  !------------------!
  !   Praparations   !
  !------------------!
  call Solvers_Mod_Prepare_System(grid)

  ! Create two full matrices from a sparse one
  call Solvers_Mod_Convert_Sparse_to_Dense(A,   a_sparse)
  call Solvers_Mod_Convert_Sparse_to_Dense(LDL, a_sparse)
  LDL % val(:,:) = 0

  ! Just print original matrix
  call In_Out_Mod_Print_Dense("A:", A)

  !------------------------!
  !   Actual computation   !
  !------------------------!

  ! Perform LDLT factorization on the matrix to fin the lower one
  call Cpu_Time(time_ps)
  call Solvers_Mod_Ldlt_Factorization_Dense(LDL, A)
  call Cpu_Time(time_pe)
  call In_Out_Mod_Print_Dense("LDL after LDL factorization", LDL)

  ! Compute x
  call Cpu_Time(time_ss)
  call Solvers_Mod_Ldlt_Solution_Dense(x, LDL, b)
  call Cpu_Time(time_se)
  !@ call In_Out_Mod_Print_Vector("Solution x:", x)

  print '(a,1es10.4)', ' # Time for matrix preparation: ', time_pe - time_ps
  print '(a,1es10.4)', ' # Time for solution:           ', time_se - time_ss
  print '(a,1es10.4)', ' # Total time:                  ',  &
                                       time_pe - time_ps + time_se - time_ss

  !------------------------!
  !   Check the solition   !
  !------------------------!
  call Solvers_Mod_Check_Solution_Dense(A)

  !-------------------------!
  !   Clean-up the memory   !
  !-------------------------!
  call Solvers_Mod_Deallocate()

  end subroutine
