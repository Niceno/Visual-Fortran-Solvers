!==============================================================================!
  subroutine Solvers_Mod_Incomplete_Ldlt_From_Tflows(grid, A, x, b)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type)   :: grid  !! computational grid
  type(Sparse_Type) :: A     !! original sparse system matrix
  real, allocatable :: x(:)
  real, allocatable :: b(:)
!-----------------------------------[Locals]-----------------------------------!
  real                       :: time_ps, time_pe, time_ss, time_se
  type(Sparse_Type), pointer :: D  ! used for LDL' factorization, but this
                                   ! one stores only the diagonal
!==============================================================================!

  ! Take aliases
  D => p_sparse

  print *, '#================================================================='
  print *, '# Solving the sytem with T-Flows'' incomplete LDL'' factorization'
  print *, '#-----------------------------------------------------------------'

  !------------------!
  !   Praparations   !
  !------------------!
  call Discretize % On_Sparse_Matrix(grid, A, x, b)
  call Solvers_Mod_Allocate_Vectors(A % n)
  call D % Sparse_Create_Preconditioning(A, 0)

  !------------------------!
  !   Actual computation   !
  !------------------------!

  ! Perform LDL' factorization on the matrix to find the lower one
  call Cpu_Time(time_ps)
  call Solvers_Mod_Tflows_Ldlt_Factorization(D, A)
  call Cpu_Time(time_pe)
  call In_Out_Mod_Print_Sparse("D after factorization:", D)

  ! Compute x
  call Cpu_Time(time_ss)
  call Solvers_Mod_Tflows_Ldlt_Solution(A % n, -1, A, D, x, b)
  call Cpu_Time(time_se)
  !@ call In_Out_Mod_Print_Vector("Solution x:", x)

  print '(a,1es10.4)', ' # Time for matrix preparation: ', time_pe - time_ps
  print '(a,1es10.4)', ' # Time for solution:           ', time_se - time_ss
  print '(a,1es10.4)', ' # Total time:                  ',  &
                                       time_pe - time_ps + time_se - time_ss

  !------------------------!
  !   Check the solution   !
  !------------------------!
  call Solvers_Mod_Check_Solution_Sparse(A, x, b)

  !-------------------------!
  !   Clean-up the memory   !
  !-------------------------!
  call Solvers_Mod_Deallocate()
  call A % Sparse_Deallocate()
  deallocate(x)
  deallocate(b)

  end subroutine
