!==============================================================================!
  subroutine Solvers_Mod_Incomplete_Cholesky(grid, A, x, b, fill_in)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type)   :: grid     !! computational grid
  type(Sparse_Type) :: A        !! original sparse system matrix
  real, allocatable :: x(:)
  real, allocatable :: b(:)
  integer           :: fill_in  !! fill-in factor
!-----------------------------------[Locals]-----------------------------------!
  real                       :: time_ps, time_pe, time_ss, time_se
  type(Sparse_Type), pointer :: LL  ! used for LL' (Cholesky) factorization
!==============================================================================!

  ! Take aliases
  LL => p_sparse

  print *, '#=========================================================='
  print *, '# Solving the sytem with incomplete Cholesky factorization'
  print *, '#----------------------------------------------------------'

  !------------------!
  !   Praparations   !
  !------------------!
  call Discretize % On_Sparse_Matrix(grid, A, x, b)
  call Solvers_Mod_Allocate_Vectors(A % n)
  call LL % Sparse_Create_Preconditioning(A, fill_in)

  !------------------------!
  !   Actual computation   !
  !------------------------!

  ! Perform Cholesky factorization on the matrix to fin the lower one
  call Cpu_Time(time_ps)
  call Solvers_Mod_Sparse_Cholesky_Factorization(LL, A)
  call Cpu_Time(time_pe)
  call In_Out_Mod_Print_Sparse("LL after factorization:", LL)

  ! Compute y by forward substitution
  call Cpu_Time(time_ss)
  call Solvers_Mod_Sparse_Forward_Substitution(y, LL, b)

  ! Compute x by backward substitution
  call Solvers_Mod_Sparse_Backward_Substitution(x, LL, y)
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
