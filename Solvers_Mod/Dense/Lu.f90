!==============================================================================!
  subroutine Solvers_Mod_Lu(Grid, A, x, b, option)
!------------------------------------------------------------------------------!
!   In a nutshell:                                                             !
!   1 - calls Lu Factorization                                                 !
!   2 - calls Forward_Substitution                                             !
!   3 - calls Backward Substitution                                            !
!------------------------------------------------------------------------------!
!   LU decomposition in full, looks like this:                                 !
!                                                                              !
!        |  1                  | | U11 U12 U13 U14 U15 |                       !
!        | L21  1              | |     U22 U23 U24 U25 |                       !
!   LU = | L31 L32  1          | |         U33 U34 U35 |                       !
!        | L41 L42 L43  1      | |             U44 U45 |                       !
!        | L51 L52 L53 L54  1  | |                 U55 |                       !
!                                                                              !
!   But given that L's diagonal is equal to one, it doesn't have to be stored: !
!                                                                              !
!              | U11 U12 U13 U14 U15 |                                         !
!     stored   | L21 U22 U23 U24 U25 |                                         !
!   LU       = | L31 L32 U33 U34 U35 |                                         !
!              | L41 L42 L43 U44 U45 |                                         !
!              | L51 L52 L53 L54 U55 |                                         !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type)     :: Grid    !! computational grid
  type(Dense_Type)    :: A       !! original dense system matrix
  real, allocatable   :: x(:)    !! solution vector
  real, allocatable   :: b(:)    !! right-hand side
  integer, intent(in) :: option  !! option for LU factorization
!-----------------------------------[Locals]-----------------------------------!
  real                      :: time_ps, time_pe, time_ss, time_se
  type(Sparse_Type)         :: H   ! helping sparse matrix for discretization
  type(Dense_Type), pointer :: LU  ! will store LU factorization
!==============================================================================!

  ! Take aliases
  LU => P_Dense

  print *, '#=========================================================='
  if(option .eq. GAUSS) then
    print *, '# Solving the sytem with Gauss-based LU factorization'
  else
    print *, '# Solving the sytem with Doolittle''s LU factorization'
  end if
  print *, '#----------------------------------------------------------'

  !------------------!
  !   Praparations   !
  !------------------!
  call Discretize % On_Sparse_Matrix(Grid, H, x, b)
  call Solvers_Mod_Allocate_Vectors(H % n)

  ! Create two full matrices from the sparse one
  call Solvers_Mod_Convert_Sparse_to_Dense(A,  H)
  call Solvers_Mod_Convert_Sparse_to_Dense(LU, H)
  LU % val(:,:) = 0

  ! Just plot and print original matrix
  call IO % Plot_Dense ("dens_a",                   A)
  call IO % Print_Dense("Dense original matrix A:", A)

  !------------------------!
  !   Actual computation   !
  !------------------------!

  ! Perform LU factorization on the matrix to fin the lower one
  call Cpu_Time(time_ps)
  if(option .eq. GAUSS) then
    call Solvers_Mod_Dense_Lu_Factorization_Gauss(LU, A)
  else
    call Solvers_Mod_Dense_Lu_Factorization_Doolittle(LU, A)
  end if
  call Cpu_Time(time_pe)

  call IO % Plot_Dense ("dens_lu_factorized",   LU)
  call IO % Print_Dense("Dense LU factorized:", LU)

  ! Compute y by forward substitution (solve: Ly=b; diagonal equal to 1)
  call Cpu_Time(time_ss)
  call Solvers_Mod_Dense_Forward_Substitution(y, LU, b, d_one=.true.)

  ! Compute x by backward substitution (solve: Ux=y)
  call Solvers_Mod_Dense_Backward_Substitution(x, LU, y)
  call Cpu_Time(time_se)

  print '(a,1es10.3)', ' # Time for matrix preparation: ', time_pe - time_ps
  print '(a,1es10.3)', ' # Time for solution:           ', time_se - time_ss
  print '(a,1es10.3)', ' # Total time:                  ',  &
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
