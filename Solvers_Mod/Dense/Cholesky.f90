!==============================================================================!
  subroutine Solvers_Mod_Cholesky(grid, A, x, b)
!------------------------------------------------------------------------------!
!   In a nutshell:                                                             !
!   1 - calls Cholesky Factorization                                           !
!   2 - calls Forward_Substitution                                             !
!   3 - calls Backward Substitution                                            !
!------------------------------------------------------------------------------!
!   Cholesky factorization in full, looks like this:                           !
!                                                                              !
!         | L11                 | | L11 L12 L13 L14 L15 |                      !
!         | L21 L22             | |     L22 L23 L24 L25 |                      !
!   LL' = | L31 L32 L33         | |         L33 L34 L35 |                      !
!         | L41 L42 L43 L44     | |             L44 L45 |                      !
!         | L51 L52 L53 L54 L55 | |                 L55 |                      !
!                                                                              !
!   But given that LL's is symmetric, only one L is stored:                    !
!                                                                              !
!              | L11                 |                                         !
!     stored   | L21 L22             |                                         !
!   LL'      = | L31 L32 L33         |                                         !
!              | L41 L42 L43 L44     |                                         !
!              | L51 L52 L53 L54 U55 |                                         !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type)   :: grid  !! computational grid
  type(Dense_Type)  :: A     !! original dense system matrix
  real, allocatable :: x(:)
  real, allocatable :: b(:)
!-----------------------------------[Locals]-----------------------------------!
  real                      :: time_ps, time_pe, time_ss, time_se
  type(Sparse_Type)         :: H  ! helping sparse matrix for discretization
  type(Dense_Type), pointer :: L  ! matrix after Cholesky factorization
!==============================================================================!

  ! Take aliases
  L => P_Dense

  print *, '#=========================================================='
  print *, '# Solving the sytem with Cholesky factorization'
  print *, '#----------------------------------------------------------'

  !------------------!
  !   Preparations   !
  !------------------!
  call Discretize % On_Sparse_Matrix(grid, H, x, b)
  call Solvers_Mod_Allocate_Vectors(H % n)

  ! Create two full matrices from a sparse
  call Solvers_Mod_Convert_Sparse_to_Dense(A, H)
  call Solvers_Mod_Convert_Sparse_to_Dense(L, H)
  L % val(:,:) = 0

  ! Just plot and print original matrix
  call IO % Plot_Dense ("a",  A)
  call IO % Print_Dense("A:", A)

  !------------------------!
  !   Actual computation   !
  !------------------------!

  ! Perform Cholesky factorization on the matrix to find the lower one
  call Cpu_Time(time_ps)
  call Solvers_Mod_Dense_Cholesky_Factorization(L, A)
  call Cpu_Time(time_pe)

  call IO % Plot_Dense ("ll_after_cholesky_factorization",  L)
  call IO % Print_Dense("LL after Cholesky factorization:", L)

  ! Compute y by forward substitution (solve: Ly=b)
  call Cpu_Time(time_ss)
  call Solvers_Mod_Dense_Forward_Substitution(y, L, b)

  ! Compute x by backward substitution (solve: Ux=y; use transposed L for U)
  call Solvers_Mod_Dense_Backward_Substitution(x, L, y, t=.true.)
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
