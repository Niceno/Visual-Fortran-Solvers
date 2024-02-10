!==============================================================================!
  subroutine Solvers_Mod_Lu(grid, A, x, b)
!------------------------------------------------------------------------------!
!   In a nutshell:                                                             !
!   1 - calls Lu Factorization                                                 !
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
  type(Sparse_Type)         :: H     ! helping sparse matrix for discretization
  type(Dense_Type), pointer :: L, U  ! will store LU factorization
!==============================================================================!

  ! Take aliases
  L => p_square
  U => q_square

  print *, '#=========================================================='
  print *, '# Solving the sytem with LU factorization'
  print *, '#----------------------------------------------------------'

  !------------------!
  !   Praparations   !
  !------------------!
  call Discretize % On_Sparse_Matrix(grid, H, x, b)
  call Solvers_Mod_Allocate_Vectors(H % n)

  ! Create two full matrices from the sparse one
  call Solvers_Mod_Convert_Sparse_to_Dense(A, H)
  call Solvers_Mod_Convert_Sparse_to_Dense(L, H)
  call Solvers_Mod_Convert_Sparse_to_Dense(U, H)
  L % val(:,:) = 0
  U % val(:,:) = 0

  ! Just plot and print original matrix
  call IO % Plot_Dense ("a",  A)
  call IO % Print_Dense("A:", A)

  !------------------------!
  !   Actual computation   !
  !------------------------!

  ! Perform LU factorization on the matrix to fin the lower one
  call Cpu_Time(time_ps)
  call Solvers_Mod_Dense_Lu_Factorization(L, U, A)
  call Cpu_Time(time_pe)

  call IO % Plot_Dense ("l_after_lu_factorization",  L)
  call IO % Plot_Dense ("u_after_lu_factorization",  U)
  call IO % Print_Dense("L after LU factorization:", L)
  call IO % Print_Dense("U after LU factorization:", U)

  ! Compute y by forward substitution: Ly=b
  call Cpu_Time(time_ss)
  call Solvers_Mod_Dense_Forward_Substitution(y, L, b)
  !@ call In_Out_Mod_Print_Vector("Vector y after forward substitution:", y)

  ! Compute x by backward substitution Ub=x
  call Solvers_Mod_Dense_Backward_Substitution(x, U, y)
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
