!==============================================================================!
  subroutine Solvers_Mod_Gauss(Grid, A, x, b)
!------------------------------------------------------------------------------!
!   In a nutshell:                                                             !
!   1 - calls Gaussian Elimination                                             !
!   2 - calls Backward Substitution                                            !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type)   :: Grid  !! computational grid
  type(Dense_Type)  :: A     !! original dense system matrix
  real, allocatable :: x(:)
  real, allocatable :: b(:)
!-----------------------------------[Locals]-----------------------------------!
  real                      :: time_ps, time_pe, time_ss, time_se
  real, allocatable         :: b_init(:)
  type(Sparse_Type)         :: H  ! helping sparse matrix for discretization
  type(Dense_Type), pointer :: U  ! original matrix (A) and matrix after
                                  ! (forward) elimination (U)
!==============================================================================!

  ! Take aliases
  U => P_Dense

  print *, '#=========================================================='
  print *, '# Solving the sytem with Gaussian elimination'
  print *, '#----------------------------------------------------------'

  !------------------!
  !   Praparations   !
  !------------------!
  call Discretize % On_Sparse_Matrix(Grid, H, x, b)
  call Solvers_Mod_Allocate_Vectors(H % n)

  ! For Gaussian elimination, we should store initial source term
  allocate(b_init(H % n))
  b_init = b

  ! Create two full matrices from a sparse
  call Solvers_Mod_Convert_Sparse_to_Dense(A, H)
  call Solvers_Mod_Convert_Sparse_to_Dense(U, H)
  U % val(:,:) = 0

  ! Just plot and print original matrix
  call IO % Plot_Dense ("dens_original",  A)
  call IO % Print_Dense("Original system matrix:", A)

  !------------------------!
  !   Actual computation   !
  !------------------------!

  ! Perform Gauss elimination on matrix and r.h.s. vector
  call Cpu_Time(time_ps)
  call Solvers_Mod_Gauss_Elimination(U, b, A)
  call Cpu_Time(time_pe)

  call IO % Plot_Dense ("dens_gauss_eliminated", U)
  call IO % Print_Dense("Gaussian elimination:", U)

  ! Perform backward substitution Ub=x
  call Cpu_Time(time_ss)
  call Solvers_Mod_Dense_Backward_Substitution(x, U, b)
  call Cpu_Time(time_se)

  print '(a,1es10.3)', ' # Time for matrix preparation: ', time_pe - time_ps
  print '(a,1es10.3)', ' # Time for solution:           ', time_se - time_ss
  print '(a,1es10.3)', ' # Total time:                  ',  &
                                       time_pe - time_ps + time_se - time_ss

  !------------------------!
  !   Check the solution   !
  !------------------------!
  call Solvers_Mod_Check_Solution_Dense(A, x, b_init)

  !-------------------------!
  !   Clean-up the memory   !
  !-------------------------!
  call Solvers_Mod_Deallocate()
  call A % Dense_Deallocate()
  deallocate(x)
  deallocate(b)

  end subroutine
