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
  type(Sparse_Type), pointer :: L  ! used for LL' (Cholesky) factorization
!==============================================================================!

  ! Take aliases
  L => P_Sparse

  print *, '#=========================================================='
  print *, '# Solving the sytem with incomplete Cholesky factorization'
  print *, '#----------------------------------------------------------'

  !------------------!
  !   Preparations   !
  !------------------!
  call Discretize % On_Sparse_Matrix(grid, A, x, b)
  call Solvers_Mod_Allocate_Vectors(A % n)
  call L % Sparse_Create_Preconditioning(A, fill_in)

  ! Just plot and print original matrix
  call IO % Plot_Sparse ("a",  A)
  call IO % Print_Sparse("A:", A)

  !------------------------!
  !   Actual computation   !
  !------------------------!

  ! Perform Cholesky factorization on the matrix to find the lower one
  call Cpu_Time(time_ps)
  call Solvers_Mod_Sparse_Cholesky_Factorization(L, A)
  call Cpu_Time(time_pe)

  call IO % Plot_Sparse ("ll_after_factorization",  L)
  call IO % Print_Sparse("LL after factorization:", L)

  ! Compute y by forward substitution
  call Cpu_Time(time_ss)
  call Solvers_Mod_Sparse_Forward_Substitution(y, L, b)

  ! Compute x by backward substitution (use L matrix but transposed)
  call Solvers_Mod_Sparse_Backward_Substitution(x, L, y, t=.true.)
  call Cpu_Time(time_se)

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
