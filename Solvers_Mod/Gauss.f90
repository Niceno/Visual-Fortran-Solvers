!==============================================================================!
  subroutine Solvers_Mod_Gauss(grid)
!------------------------------------------------------------------------------!
!   In a nutshell:                                                             !
!   1 - calls Gaussian Elimination                                             !
!   2 - calls Backward Substitution                                            !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type) :: grid
!-----------------------------------[Locals]-----------------------------------!
  real                      :: time_ps, time_pe, time_ss, time_se
  integer                   :: bw
  type(Dense_Type), pointer :: A, U  ! original matrix (A) and matrix after
                                     ! (forward) elimination (U)
!==============================================================================!

  print *, '#=========================================================='
  print *, '# Solving the sytem with Gaussian elimination'
  print *, '#----------------------------------------------------------'

  A => a_square
  U => q_square

  !------------------!
  !   Praparations   !
  !------------------!
  call Solvers_Mod_Prepare_System(grid)

  ! Create two full matrices from a sparse
  call Solvers_Mod_Convert_Sparse_to_Dense(A, a_sparse, bw)
  call Solvers_Mod_Convert_Sparse_to_Dense(U, a_sparse, bw)
  U % val(:,:) = 0

  ! Just print original matrix
  call In_Out_Mod_Print_Dense("A:", A)

  !------------------------!
  !   Actual computation   !
  !------------------------!

  ! Perform Gauss elimination on matrix and r.h.s. vector
  call Cpu_Time(time_ps)
  call Solvers_Mod_Gauss_Elimination(U, b, A, bw)
  call Cpu_Time(time_pe)
  call In_Out_Mod_Print_Dense("U after elimination:", U)
  !@ call In_Out_Mod_Print_Vector("vector b after elimination:", b)

  ! Perform backward substitution Ub=x
  call Cpu_Time(time_ss)
  call Solvers_Mod_Backward_Substitution_Dense(x, U, b)
  call Cpu_Time(time_se)
  !@ call In_Out_Mod_Print_Vector("Solution x after backward substitution:", x)

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
