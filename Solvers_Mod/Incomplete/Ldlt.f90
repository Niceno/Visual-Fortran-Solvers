!==============================================================================!
  subroutine Solvers_Mod_Incomplete_Ldlt(grid, A, x, b, fill_in)
!------------------------------------------------------------------------------!
!   LDL' decomposition in full, looks like this:                               !
!                                                                              !
!   LDL =                                                                      !
!    |  1                  | | D11                 | |  1  L12 L13         |   !
!    | L21  1              | |     D22             | |      1  L23 L24     |   !
!    | L31 L32  1          | |         D33         | |          1  L34 L35 |   !
!    |     L42 L43  1      | |             D44     | |              1  L45 |   !
!    |         L53 L54  1  | |                 D55 | |                  1  |   !
!                                                                              !
!   But given that L's diagonal is equal to one, it doesn't have to be stored. !
!                                                                              !
!               | D11                 |                                        !
!      stored   | L21 D22             |                                        !
!   LDL       = | L31 L32 D33         |                                        !
!               |     L42 L43 D44     |                                        !
!               |         L53 L54 D55 |                                        !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type)   :: grid     !! computational grid
  type(Sparse_Type) :: A        !! original sparse system matrix
  real, allocatable :: x(:)     !! unknown
  real, allocatable :: b(:)     !! right hand side vector
  integer           :: fill_in  !! fill-in factor
!-----------------------------------[Locals]-----------------------------------!
  real                       :: time_ps, time_pe, time_ss, time_se
  type(Sparse_Type), pointer :: LD  ! used for LDL' factorization
!==============================================================================!

  ! Take aliases
  LD => P_Sparse

  print *, '#=========================================================='
  print *, '# Solving the sytem with incomplete LDL'' factorization'
  print *, '#----------------------------------------------------------'

  !------------------!
  !   Praparations   !
  !------------------!
  call Discretize % On_Sparse_Matrix(grid, A, x, b)
  call Solvers_Mod_Allocate_Vectors(A % n)
  call LD % Sparse_Create_Preconditioning(A, fill_in)

  !------------------------!
  !   Actual computation   !
  !------------------------!

  ! Perform LDL' factorization on the matrix to fin the lower one
  call Cpu_Time(time_ps)
  call Solvers_Mod_Sparse_Ldlt_Factorization(LD, A)
  call Cpu_Time(time_pe)

  call IO % Plot_Sparse ("ldl_after_factorization",   LD)
  call IO % Print_Sparse("LDL' after factorization:", LD)

  ! Compute x
  call Cpu_Time(time_ss)
  call Solvers_Mod_Sparse_Ldlt_Solution(x, LD, b)
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
