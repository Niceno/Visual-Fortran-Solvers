!==============================================================================!
  subroutine Solvers_Mod_Cholesky(grid)
!------------------------------------------------------------------------------!
!   In a nutshell:                                                             !
!   1 - calls Cholesky Factorization                                           !
!   2 - calls Forward_Substitution                                             !
!   3 - calls Backward Substitution                                            !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type) :: grid
!-----------------------------------[Locals]-----------------------------------!
  real                      :: time_ps, time_pe, time_ss, time_se
  integer                   :: bw
  type(Dense_Type), pointer :: A, LL
!==============================================================================!

  print *, '#=========================================================='
  print *, '# Solving the sytem with Cholesky decomposition'
  print *, '#----------------------------------------------------------'

  A  => a_square
  LL => p_square

  !------------------!
  !   Praparations   !
  !------------------!
  call Solvers_Mod_Prepare_System(grid)

  ! Create two full matrices from the sparse one
  call Solvers_Mod_Convert_Sparse_to_Dense(A,  a_sparse, bw)
  call Solvers_Mod_Convert_Sparse_to_Dense(LL, a_sparse, bw)
  LL % val(:,:) = 0

  ! Just print original matrix
  call In_Out_Mod_Print_Dense("A:", A)

  !------------------------!
  !   Actual computation   !
  !------------------------!

  ! Perform Cholesky factorization on the matrix to fin the lower one
  call Cpu_Time(time_ps)
  call Solvers_Mod_Cholesky_Factorization_Dense(LL, A, bw)
  call Cpu_Time(time_pe)
  call In_Out_Mod_Print_Dense("LL after Cholesky factorization", LL)

  ! Compute y by forward substitution
  call Cpu_Time(time_ss)
  call Solvers_Mod_Forward_Substitution_Dense(y, LL, b)
  !@ call In_Out_Mod_Print_Vector("Vector y after forward substitution:", y)

  ! Compute x by backward substitution
  call Solvers_Mod_Backward_Substitution_Dense(x, LL, y)
  !@ call In_Out_Mod_Print_Vector("Solution x after backward substitution:", x)
  call Cpu_Time(time_se)

  print '(a,1es10.4)', ' # Time for matrix preparation: ', time_pe - time_ps
  print '(a,1es10.4)', ' # Time for solution:           ', time_se - time_ss
  print '(a,1es10.4)', ' # Total time:                  ',  &
                                       time_pe - time_ps + time_se - time_ss

  !------------------------!
  !   Check the solution   !
  !------------------------!
  call Solvers_Mod_Check_Solution_Dense(A)

  !-------------------------!
  !   Clean-up the memory   !
  !-------------------------!
  call Solvers_Mod_Deallocate()

  end subroutine
