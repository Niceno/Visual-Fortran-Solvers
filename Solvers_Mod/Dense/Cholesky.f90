!==============================================================================!
  subroutine Solvers_Mod_Cholesky(grid, A, x, b)
!------------------------------------------------------------------------------!
!   In a nutshell:                                                             !
!   1 - calls Cholesky Factorization                                           !
!   2 - calls Forward_Substitution                                             !
!   3 - calls Backward Substitution                                            !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type)  :: grid   !! computational grid
  type(Dense_Type) :: A      !! original dense system matrix
  real, allocatable :: x(:)
  real, allocatable :: b(:)
!-----------------------------------[Locals]-----------------------------------!
  real                      :: time_ps, time_pe, time_ss, time_se
  type(Sparse_Type)         :: H   ! helping sparse matrix for discretization
  type(Dense_Type), pointer :: LL  ! matrix after Cholesky factorization
!==============================================================================!

  ! Take aliases
  LL => q_square

  print *, '#=========================================================='
  print *, '# Solving the sytem with Cholesky factorization'
  print *, '#----------------------------------------------------------'

  !------------------!
  !   Praparations   !
  !------------------!
  call Discretize % On_Sparse_Matrix(grid, H, x, b)
  call Solvers_Mod_Prepare_System(grid)

  ! Create two full matrices from a sparse
  call Solvers_Mod_Convert_Sparse_to_Dense(A,  H)
  call Solvers_Mod_Convert_Sparse_to_Dense(LL, H)
  LL % val(:,:) = 0

  ! Just print original matrix
  call In_Out_Mod_Print_Dense("A:", A)

  !------------------------!
  !   Actual computation   !
  !------------------------!

  ! Perform Cholesky factorization on the matrix to fin the lower one
  call Cpu_Time(time_ps)
  call Solvers_Mod_Dense_Cholesky_Factorization(LL, A)
  call Cpu_Time(time_pe)
  call In_Out_Mod_Print_Dense("LL after Cholesky factorization", LL)

  ! Compute y by forward substitution
  call Cpu_Time(time_ss)
  call Solvers_Mod_Dense_Forward_Substitution(y, LL, b)
  !@ call In_Out_Mod_Print_Vector("Vector y after forward substitution:", y)

  ! Compute x by backward substitution
  call Solvers_Mod_Dense_Backward_Substitution(x, LL, y)
  !@ call In_Out_Mod_Print_Vector("Solution x after backward substitution:", x)
  call Cpu_Time(time_se)

  print '(a,1es10.4)', ' # Time for matrix preparation: ', time_pe - time_ps
  print '(a,1es10.4)', ' # Time for solution:           ', time_se - time_ss
  print '(a,1es10.4)', ' # Total time:                  ',  &
                                       time_pe - time_ps + time_se - time_ss

  !------------------------!
  !   Check the solution   !
  !------------------------!
  call Solvers_Mod_Check_Solution_Dense(A, x, b)

  !-------------------------!
  !   Clean-up the memory   !
  !-------------------------!
  call Solvers_Mod_Deallocate()
  call A % Dense_Deallocate()
  deallocate(x)
  deallocate(b)

  end subroutine
