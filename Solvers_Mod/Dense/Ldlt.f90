!==============================================================================!
  subroutine Solvers_Mod_Ldlt(grid, A, x, b)
!------------------------------------------------------------------------------!
!   In a nutshell:                                                             !
!   1 - calls Ldlt Factorization                                               !
!   2 - calls Ldlt Solution                                                    !
!------------------------------------------------------------------------------!
!   LDL' decomposition in full, looks like this:                               !
!                                                                              !
!   LDL =                                                                      !
!    |  1                  | | D11                 | |  1  L12 L13 L14 L15 |   !
!    | L21  1              | |     D22             | |      1  L23 L24 L25 |   !
!    | L31 L32  1          | |         D33         | |          1  L34 L35 |   !
!    | L41 L42 L43  1      | |             D44     | |              1  L45 |   !
!    | L51 L52 L53 L54  1  | |                 D55 | |                  1  |   !
!                                                                              !
!   But given that L's diagonal is equal to one, it doesn't have to be stored. !
!                                                                              !
!               | D11                 |                                        !
!      stored   | L21 D22             |                                        !
!   LDL       = | L31 L32 D33         |                                        !
!               | L41 L42 L43 D44     |                                        !
!               | L51 L52 L53 L54 D55 |                                        !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type)   :: grid  !! computational grid
  type(Dense_Type)  :: A     !! original dense system matrix
  real, allocatable :: x(:)
  real, allocatable :: b(:)
!-----------------------------------[Locals]-----------------------------------!
  real                      :: time_ps, time_pe, time_ss, time_se
  type(Sparse_Type)         :: H    ! helping sparse matrix for discretization
  type(Dense_Type), pointer :: LDL  ! used for LDL' factorization
!==============================================================================!

  ! Take aliases
  LDL => P_Dense

  print *, '#=========================================================='
  print *, '# Solving the sytem with LDL'' factorization'
  print *, '#----------------------------------------------------------'

  !------------------!
  !   Praparations   !
  !------------------!
  call Discretize % On_Sparse_Matrix(grid, H, x, b)
  call Solvers_Mod_Allocate_Vectors(H % n)

  ! Create two full matrices from a sparse
  call Solvers_Mod_Convert_Sparse_to_Dense(A,   H)
  call Solvers_Mod_Convert_Sparse_to_Dense(LDL, H)
  LDL % val(:,:) = 0

  ! Just plot and print original matrix
  call IO % Plot_Dense ("a",  A)
  call IO % Print_Dense("A:", A)

  !------------------------!
  !   Actual computation   !
  !------------------------!

  ! Perform LDLT factorization on the matrix to fin the lower one
  call Cpu_Time(time_ps)
  call Solvers_Mod_Dense_Ldlt_Factorization(LDL, A)
  call Cpu_Time(time_pe)

  call IO % Plot_Dense ("ldl_after_factorization",  LDL)
  call IO % Print_Dense("LDL after factorization:", LDL)

  ! Compute x
  call Cpu_Time(time_ss)
  call Solvers_Mod_Dense_Ldlt_Solution(x, LDL, b)
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
