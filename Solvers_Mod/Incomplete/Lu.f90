!==============================================================================!
  subroutine Solvers_Mod_Incomplete_Lu(grid, A, x, b, fill_in, option)
!------------------------------------------------------------------------------!
!   LU decomposition in full, looks like this:                                 !
!                                                                              !
!        |  1                  | | U11 U12 U13         |                       !
!        | L21  1              | |     U22 U23 U24     |                       !
!   LU = | L31 L32  1          | |         U33 U34 U35 |                       !
!        |     L42 L43  1      | |             U44 U45 |                       !
!        |         L53 L54  1  | |                 U55 |                       !
!                                                                              !
!   But given that L's diagonal is equal to one, it doesn't have to be stored: !
!                                                                              !
!              | U11 U12 U13         |                                         !
!     stored   | L21 U22 U23 U24     |                                         !
!   LU       = | L31 L32 U33 U34 U35 |                                         !
!              |     L42 L43 U44 U45 |                                         !
!              |         L53 L54 U55 |                                         !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type)     :: grid     !! computational grid
  type(Sparse_Type)   :: A        !! original sparse system matrix
  real, allocatable   :: x(:)     !! unknown
  real, allocatable   :: b(:)     !! right hand side vector
  integer             :: fill_in  !! fill-in factor
  integer, intent(in) :: option   !! option for LU factorization
!-----------------------------------[Locals]-----------------------------------!
  real                       :: time_ps, time_pe, time_ss, time_se
  type(Sparse_Type), pointer :: LU  ! used for LDL' factorization
!==============================================================================!

  ! Take aliases
  LU => P_Sparse

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
  call Discretize % On_Sparse_Matrix(grid, A, x, b)
  call Solvers_Mod_Allocate_Vectors(A % n)
  call LU % Sparse_Create_Preconditioning(A, fill_in)

  !------------------------!
  !   Actual computation   !
  !------------------------!

  ! Perform LU factorization on the matrix to fin the lower one
  call Cpu_Time(time_ps)
  if(option .eq. GAUSS) then
    call Solvers_Mod_Sparse_Lu_Factorization_Gauss(LU, A)
  else
    call Solvers_Mod_Sparse_Lu_Factorization_Doolittle(LU, A)
  end if
  call Cpu_Time(time_pe)

  call IO % Plot_Sparse ("spar_lu_factorized",    LU)
  call IO % Print_Sparse("Sparse LU factorized:", LU)

  ! Compute y by forward substitution (solve: Ly=b; diagonal equal to 1)
  call Cpu_Time(time_ss)
  call Solvers_Mod_Sparse_Forward_Substitution(y, LU, b, d_one=.true.)

  ! Compute x by backward substitution (solve: Ux=y)
  call Solvers_Mod_Sparse_Backward_Substitution(x, LU, y)
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
